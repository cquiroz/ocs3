// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server.ghost

import cats.implicits._
import cats.Eq
import gem.enum.GiapiStatusApply._
import gsp.math.Coordinates
import giapi.client.commands.Configuration
import giapi.client.GiapiConfig
import giapi.client.syntax.giapiconfig._
import scala.concurrent.duration._
import seqexec.server.ConfigUtilOps.ContentError
import seqexec.server.ConfigUtilOps.ExtractFailure

// GHOST has a number of different possible configuration modes: we add types for them here.
sealed trait GhostConfig {
  def baseCoords: Option[Coordinates]
  def expTime: Duration

  def fiberAgitator1: FiberAgitator
  def fiberAgitator2: FiberAgitator
  def ifu1TargetType: IFUTargetType
  def ifu2TargetType: IFUTargetType
  def ifu1BundleType: BundleConfig
  def ifu1Coordinates: Coordinates
  def ifu2BundleType: Option[BundleConfig]

  def ifu1Config: Configuration =
    GhostConfig.ifuConfig(IFUNum.IFU1, ifu1TargetType, ifu1Coordinates, ifu1BundleType)
  def ifu2Config: Configuration

  def configuration: Configuration =
    GhostConfig.fiberConfig1(fiberAgitator1) |+|
      GhostConfig.fiberConfig2(fiberAgitator2) |+|
      ifu1Config |+| ifu2Config

}

// These are the parameters passed to GHOST from the WDBA.
// We use them to determine the type of configuration being used by GHOST, and instantiate it.
object GhostConfig {
  private[ghost] def ifuConfig(
    ifuNum:        IFUNum,
    ifuTargetType: IFUTargetType,
    coordinates:   Coordinates,
    bundleConfig:  BundleConfig
  ): Configuration = {
    def cfg[P: GiapiConfig](paramName: String, paramVal: P) =
      Configuration.single(s"${ifuNum.configValue}.$paramName", paramVal)
    println(bundleConfig)
    val demand: DemandType = DemandType.DemandXY

    val a = cfg("target", ifuTargetType.configValue) |+|
      cfg("type", demand) |+|
      cfg("fpx", coordinates.ra.toAngle.toDoubleDegrees) |+|
      cfg("fpy", coordinates.dec.toAngle.toDoubleDegrees) |+|
      cfg("bundle", bundleConfig.configValue)
    a
  }

  private[ghost] def ifuPark(ifuNum: IFUNum): Configuration = {
    def cfg[P: GiapiConfig](paramName: String, paramVal: P) =
      Configuration.single(s"${ifuNum.ifuStr}.$paramName", paramVal)

    cfg("target", IFUTargetType.NoTarget: IFUTargetType) |+|
      cfg("type", DemandType.DemandPark: DemandType)
  }

  private[ghost] def fiberConfig1(fa: FiberAgitator): Configuration =
    Configuration.single(GhostFiberAgitator1.applyItem, fa)

  private[ghost] def fiberConfig2(fa: FiberAgitator): Configuration =
    Configuration.single(GhostFiberAgitator2.applyItem, fa)

  def apply(
    baseCoords:     Option[Coordinates],
    expTime:        Duration,
    fiberAgitator1: FiberAgitator,
    fiberAgitator2: FiberAgitator,
    srifu1Name:     Option[String],
    srifu1Coords:   Option[Coordinates],
    srifu2Name:     Option[String],
    srifu2Coords:   Option[Coordinates],
    hrifu1Name:     Option[String],
    hrifu1Coords:   Option[Coordinates],
    hrifu2Name:     Option[String],
    hrifu2Coords:   Option[Coordinates]
  ): Either[ExtractFailure, GhostConfig] = {
    import IFUTargetType._

    val sifu1 = determineType(srifu1Name)
    val sifu2 = determineType(srifu2Name)
    val hifu1 = determineType(hrifu1Name)
    val hifu2 = determineType(hrifu2Name)
    val extracted = (sifu1, sifu2, hifu1, hifu2) match {
      case (Target(t), NoTarget, NoTarget, NoTarget) =>
        srifu1Coords.map(
          StandardResolutionMode
            .SingleTarget(baseCoords, expTime, fiberAgitator1, fiberAgitator2, t, _)
        )
      case (Target(t1), Target(t2), NoTarget, NoTarget) =>
        (srifu1Coords, srifu2Coords).mapN(
          StandardResolutionMode
            .DualTarget(baseCoords, expTime, fiberAgitator1, fiberAgitator2, t1, _, t2, _)
        )
      case (Target(t), SkyPosition, NoTarget, NoTarget) =>
        (srifu1Coords, srifu2Coords).mapN(
          StandardResolutionMode
            .TargetPlusSky(baseCoords, expTime, fiberAgitator1, fiberAgitator2, t, _, _)
        )
      case (SkyPosition, Target(t), NoTarget, NoTarget) =>
        (srifu1Coords, srifu2Coords).mapN(
          StandardResolutionMode
            .SkyPlusTarget(baseCoords, expTime, fiberAgitator1, fiberAgitator2, _, t, _)
        )
      case (NoTarget, NoTarget, Target(t), NoTarget) =>
        hrifu1Coords.map(
          HighResolutionMode.SingleTarget(baseCoords, expTime, fiberAgitator1, fiberAgitator2, t, _)
        )
      case (NoTarget, NoTarget, Target(t), SkyPosition) =>
        (hrifu1Coords, hrifu2Coords).mapN(
          HighResolutionMode
            .TargetPlusSky(baseCoords, expTime, fiberAgitator1, fiberAgitator2, t, _, _)
        )
      case _ =>
        None
    }
    extracted match {
      case Some(config) => Right(config)
      case None         => Left(ContentError("Response does not constitute a valid GHOST configuration"))
    }
  }

  implicit val eq: Eq[GhostConfig] = Eq.instance {
    case (a: StandardResolutionMode.SingleTarget, b: StandardResolutionMode.SingleTarget) => a === b
    case (a: StandardResolutionMode.DualTarget, b: StandardResolutionMode.DualTarget)     => a === b
    case (a: StandardResolutionMode.TargetPlusSky, b: StandardResolutionMode.TargetPlusSky) =>
      a === b
    case (a: StandardResolutionMode.SkyPlusTarget, b: StandardResolutionMode.SkyPlusTarget) =>
      a === b
    case (a: HighResolutionMode.SingleTarget, b: HighResolutionMode.SingleTarget)   => a === b
    case (a: HighResolutionMode.TargetPlusSky, b: HighResolutionMode.TargetPlusSky) => a === b
    case _                                                                          => false
  }

}

sealed trait StandardResolutionMode extends GhostConfig {
  import StandardResolutionMode._

  def ifu1Coordinates: Coordinates

  override def ifu1TargetType: IFUTargetType = this match {
    case s: SingleTarget   => IFUTargetType.Target(s.ifu1TargetName)
    case d: DualTarget     => IFUTargetType.Target(d.ifu1TargetName)
    case ts: TargetPlusSky => IFUTargetType.Target(ts.ifu1TargetName)
    case _: SkyPlusTarget  => IFUTargetType.SkyPosition
  }

  override def ifu2TargetType: IFUTargetType = this match {
    case _: SingleTarget   => IFUTargetType.NoTarget
    case d: DualTarget     => IFUTargetType.Target(d.ifu2TargetName)
    case st: SkyPlusTarget => IFUTargetType.Target(st.ifu2TargetName)
    case _: TargetPlusSky  => IFUTargetType.SkyPosition
  }

  override def ifu1BundleType: BundleConfig = this match {
    case _: SingleTarget | _: DualTarget | _: TargetPlusSky => BundleConfig.Standard
    case _: SkyPlusTarget                                   => BundleConfig.Sky
  }

  override def ifu2BundleType: Option[BundleConfig] = this match {
    case _: SingleTarget                  => None
    case _: DualTarget | _: SkyPlusTarget => Some(BundleConfig.Standard)
    case _: TargetPlusSky                 => Some(BundleConfig.Sky)
  }
}

object StandardResolutionMode {
  final case class SingleTarget(
    override val baseCoords:      Option[Coordinates],
    override val expTime:         Duration,
    override val fiberAgitator1:  FiberAgitator,
    override val fiberAgitator2:  FiberAgitator,
    ifu1TargetName:               String,
    override val ifu1Coordinates: Coordinates
  ) extends StandardResolutionMode {
    override def ifu2Config: Configuration =
      GhostConfig.ifuPark(IFUNum.IFU2)
  }

  implicit val srmSingleTargetEq: Eq[SingleTarget] = Eq.by(x =>
    (x.baseCoords,
     x.expTime,
     x.fiberAgitator1,
     x.fiberAgitator2,
     x.ifu1TargetName,
     x.ifu1Coordinates)
  )

  final case class DualTarget(
    override val baseCoords:      Option[Coordinates],
    override val expTime:         Duration,
    override val fiberAgitator1:  FiberAgitator,
    override val fiberAgitator2:  FiberAgitator,
    ifu1TargetName:               String,
    override val ifu1Coordinates: Coordinates,
    ifu2TargetName:               String,
    ifu2Coordinates:              Coordinates
  ) extends StandardResolutionMode {
    override def ifu2Config: Configuration =
      GhostConfig.ifuConfig(IFUNum.IFU2,
                            IFUTargetType.Target(ifu2TargetName),
                            ifu2Coordinates,
                            BundleConfig.Standard)
  }

  implicit val srmDualTargetEq: Eq[DualTarget] = Eq.by(x =>
    (x.baseCoords,
     x.expTime,
     x.fiberAgitator1,
     x.fiberAgitator2,
     x.ifu1TargetName,
     x.ifu1Coordinates,
     x.ifu2TargetName,
     x.ifu2Coordinates)
  )

  final case class TargetPlusSky(
    override val baseCoords:      Option[Coordinates],
    override val expTime:         Duration,
    override val fiberAgitator1:  FiberAgitator,
    override val fiberAgitator2:  FiberAgitator,
    ifu1TargetName:               String,
    override val ifu1Coordinates: Coordinates,
    ifu2Coordinates:              Coordinates
  ) extends StandardResolutionMode {
    override def ifu2Config: Configuration =
      GhostConfig.ifuConfig(IFUNum.IFU2,
                            IFUTargetType.SkyPosition,
                            ifu2Coordinates,
                            BundleConfig.Sky)
  }

  implicit val srmTargetPlusSkyEq: Eq[TargetPlusSky] = Eq.by(x =>
    (x.baseCoords,
     x.expTime,
     x.fiberAgitator1,
     x.fiberAgitator2,
     x.ifu1TargetName,
     x.ifu1Coordinates,
     x.ifu2Coordinates)
  )

  final case class SkyPlusTarget(
    override val baseCoords:      Option[Coordinates],
    override val expTime:         Duration,
    override val fiberAgitator1:  FiberAgitator,
    override val fiberAgitator2:  FiberAgitator,
    override val ifu1Coordinates: Coordinates,
    ifu2TargetName:               String,
    ifu2Coordinates:              Coordinates
  ) extends StandardResolutionMode {
    override def ifu2Config: Configuration =
      GhostConfig.ifuConfig(IFUNum.IFU2,
                            IFUTargetType.Target(ifu2TargetName),
                            ifu2Coordinates,
                            BundleConfig.Standard)

  }

  implicit val srmSkyPlusTargetEq: Eq[SkyPlusTarget] = Eq.by(x =>
    (x.baseCoords,
     x.expTime,
     x.fiberAgitator1,
     x.fiberAgitator2,
     x.ifu1Coordinates,
     x.ifu2TargetName,
     x.ifu2Coordinates)
  )
}

sealed trait HighResolutionMode extends GhostConfig {
  import HighResolutionMode._

  override def ifu1BundleType: BundleConfig = BundleConfig.HighRes

  def ifu1TargetName: String

  override def ifu1TargetType: IFUTargetType = IFUTargetType.Target(ifu1TargetName)

  override def ifu2TargetType: IFUTargetType = this match {
    case _: SingleTarget  => IFUTargetType.NoTarget
    case _: TargetPlusSky => IFUTargetType.SkyPosition
  }

  override def ifu2BundleType: Option[BundleConfig] = this match {
    case _: SingleTarget  => None
    case _: TargetPlusSky => Some(BundleConfig.Sky)
  }
}

object HighResolutionMode {
  final case class SingleTarget(
    override val baseCoords:      Option[Coordinates],
    override val expTime:         Duration,
    override val fiberAgitator1:  FiberAgitator,
    override val fiberAgitator2:  FiberAgitator,
    override val ifu1TargetName:  String,
    override val ifu1Coordinates: Coordinates
  ) extends HighResolutionMode {
    override def ifu2Config: Configuration =
      GhostConfig.ifuPark(IFUNum.IFU2)
  }

  implicit val hrSingleTargetEq: Eq[SingleTarget] = Eq.by(x =>
    (x.baseCoords,
     x.expTime,
     x.fiberAgitator1,
     x.fiberAgitator2,
     x.ifu1TargetName,
     x.ifu1Coordinates)
  )

  final case class TargetPlusSky(
    override val baseCoords:      Option[Coordinates],
    override val expTime:         Duration,
    override val fiberAgitator1:  FiberAgitator,
    override val fiberAgitator2:  FiberAgitator,
    override val ifu1TargetName:  String,
    override val ifu1Coordinates: Coordinates,
    ifu2Coordinates:              Coordinates
  ) extends HighResolutionMode {
    override def ifu2Config: Configuration =
      GhostConfig.ifuConfig(IFUNum.IFU2,
                            IFUTargetType.SkyPosition,
                            ifu2Coordinates,
                            BundleConfig.Sky)
  }

  implicit val hrTargetPlusSkyEq: Eq[TargetPlusSky] = Eq.by(x =>
    (x.baseCoords,
     x.expTime,
     x.fiberAgitator1,
     x.fiberAgitator2,
     x.ifu1TargetName,
     x.ifu1Coordinates,
     x.ifu2Coordinates)
  )
}
