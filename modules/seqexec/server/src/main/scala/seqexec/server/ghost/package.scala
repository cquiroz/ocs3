package seqexec.server.ghost

import gem.util.Enumerated
import giapi.client.GiapiConfig

sealed trait FiberAgitator extends Product with Serializable

object FiberAgitator {
  case object On extends FiberAgitator
  case object Off extends FiberAgitator

  implicit val FiberAgitatorEnumerated: Enumerated[FiberAgitator] =
    Enumerated.of(On, Off)

  implicit val FiberAgitatorGiapi: GiapiConfig[FiberAgitator] =
    GiapiConfig.instance {
      case On  => "1"
      case Off => "0"
    }

  def fromBoolean(b: Boolean): FiberAgitator =
    if (b) On else Off
}

sealed trait DemandType extends Product with Serializable {
  def demandType: String
}

object DemandType {
  case object DemandRADec extends DemandType {
    val demandType = "IFU_DEMAND_RADEC"
  }
  case object DemandXY extends DemandType {
    val demandType = "IFU_DEMAND_XY"
  }
  case object DemandPark extends DemandType {
    val demandType = "IFU_DEMAND_PARK"
  }
  implicit val FiberAgitatorEnumerated: Enumerated[DemandType] =
    Enumerated.of(DemandRADec, DemandXY, DemandPark)

  implicit val DemandTypeCconfiguration: GiapiConfig[DemandType] =
    GiapiConfig.instance(_.demandType)
}

sealed abstract class IFUNum(val ifuNum: Int) extends Product with Serializable {
  val ifuStr: String = s"ghost:cc:cu:ifu$ifuNum"
}

object IFUNum {
  case object IFU1 extends IFUNum(ifuNum = 1)
  case object IFU2 extends IFUNum(ifuNum = 2)
  implicit val ifuNumConfiguration: GiapiConfig[IFUNum] = _.ifuStr
}

sealed abstract class BundleConfig(val configName: String) extends Product with Serializable {
  def determineType(t: IFUTargetType): BundleConfig = t match {
    case IFUTargetType.SkyPosition => BundleConfig.Sky
    case _                         => this
  }
}

object BundleConfig {
  case object Standard extends BundleConfig(configName = "IFU_LORES")
  case object HighRes extends BundleConfig(configName  = "IFU_HIRES")
  case object Sky extends BundleConfig(configName      = "IFU_SKY")
  implicit val bundleConfiguration: GiapiConfig[BundleConfig] = {
    case Standard => "0"
    case HighRes  => "1"
    case Sky      => "2"
  }
}

sealed abstract class IFUTargetType(val targetType: String) extends Product with Serializable
object IFUTargetType {

  case object NoTarget extends IFUTargetType(targetType    = "IFU_TARGET_NONE")
  case object SkyPosition extends IFUTargetType(targetType = "IFU_TARGET_SKY")
  final case class Target(name: String) extends IFUTargetType(targetType = "IFU_TARGET_OBJECT")

  def determineType(name: Option[String]): IFUTargetType = name match {
    case None        => NoTarget
    case Some("Sky") => SkyPosition
    case Some(x)     => Target(x)
  }

  implicit val ifuTargetTypeConfiguration: GiapiConfig[IFUTargetType] = {
    case NoTarget    => "0"
    case SkyPosition => "1"
    case Target(_)   => "2"
  }
}
