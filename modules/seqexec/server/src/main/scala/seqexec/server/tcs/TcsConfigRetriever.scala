// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server.tcs

import cats.data.{OneAnd, OptionT}
import cats.MonadError
import cats.effect.IO
import cats.implicits._
import mouse.boolean._
import edu.gemini.seqexec.server.tcs.{BinaryOnOff, BinaryYesNo}
import edu.gemini.spModel.core.Wavelength
import seqexec.server.EpicsCodex.{DecodeEpicsValue, decode}
import seqexec.server.tcs.TcsController.FollowOption.{FollowOff, FollowOn}
import seqexec.server.tcs.TcsController.MountGuideOption.{MountGuideOff, MountGuideOn}
import seqexec.server.SeqexecFailure
import seqexec.server.tcs.TcsController._
import seqexec.server.tcs.TcsControllerEpics.{AoFold, EpicsTcsConfig, InstrumentPorts, ScienceFold, InvalidPort}
import shapeless.tag
import squants.{Angle, Length}
import squants.space.{Angstroms, Degrees, Millimeters}

object TcsConfigRetriever {

  // Code to retrieve the current configuration from TCS. Include a lot of decoders
  implicit private val decodeMountGuideOption: DecodeEpicsValue[Int, MountGuideOption] = DecodeEpicsValue((d: Int)
  => if (d === 0) MountGuideOff else MountGuideOn)

  implicit private val decodeM1GuideSource: DecodeEpicsValue[String, M1Source] = DecodeEpicsValue((s: String)
  => s.trim match {
      case "PWFS1" => M1Source.PWFS1
      case "PWFS2" => M1Source.PWFS2
      case "OIWFS" => M1Source.OIWFS
      case "GAOS"  => M1Source.GAOS
      case _       => M1Source.PWFS1
    })

  private def decodeM1Guide(r: BinaryOnOff, s: M1Source): M1GuideConfig =
    if (r === BinaryOnOff.Off) M1GuideOff
    else M1GuideOn(s)

  private def decodeGuideSourceOption(s: String): Boolean = s.trim =!= "OFF"

  implicit private val decodeComaOption: DecodeEpicsValue[String, ComaOption] = DecodeEpicsValue((s: String)
  => if (s.trim === "Off") ComaOption.ComaOff else ComaOption.ComaOn)

  private def decodeM2Guide(s: BinaryOnOff, u: ComaOption, v: Set[TipTiltSource]): M2GuideConfig =
    if (s === BinaryOnOff.Off) M2GuideOff
    else M2GuideOn(u, v)

  implicit private val decodeAoFold: DecodeEpicsValue[String, AoFold] = DecodeEpicsValue((s: String) =>
    if(s.trim === "IN") AoFold.In
    else AoFold.Out
  )

  private def getGuideConfig: IO[TelescopeGuideConfig] =
    (for {
      mountGuide <-  TcsEpics.instance.absorbTipTilt.map(decode[Int, MountGuideOption])
      m1Source   <-  TcsEpics.instance.m1GuideSource.map(decode[String, M1Source])
      m1Guide    <-  TcsEpics.instance.m1Guide.map(decodeM1Guide(_, m1Source))
      m2p1Guide  <-  TcsEpics.instance.m2p1Guide.map(decodeGuideSourceOption)
      m2p2Guide  <-  TcsEpics.instance.m2p2Guide.map(decodeGuideSourceOption)
      m2oiGuide  <-  TcsEpics.instance.m2oiGuide.map(decodeGuideSourceOption)
      m2aoGuide  <-  TcsEpics.instance.m2aoGuide.map(decodeGuideSourceOption)
      m2Coma     <-  TcsEpics.instance.comaCorrect.map(decode[String, ComaOption])
      m2Guide    <-  TcsEpics.instance.m2GuideState.map(decodeM2Guide(_, m2Coma, List((m2p1Guide, TipTiltSource.PWFS1),
        (m2p2Guide, TipTiltSource.PWFS2), (m2oiGuide, TipTiltSource.OIWFS),
        (m2aoGuide, TipTiltSource.GAOS)).foldLeft(Set.empty[TipTiltSource])((s: Set[TipTiltSource], v: (Boolean, TipTiltSource)) => if (v._1) s + v._2 else s)))
    } yield TelescopeGuideConfig(mountGuide, m1Guide, m2Guide)
    ).adaptError {
      case _ => SeqexecFailure.Unexpected("Unable to read guide configuration from TCS.")
    }

  private def getAoFold: IO[AoFold] =
    getStatusVal(TcsEpics.instance.aoFoldPosition, "AO Fold").map(decode[String, AoFold])

  private def decodeNodChopOption(s: String): Boolean = s.trim === "On"

  private def getNodChopTrackingConfig(g: TcsEpics.ProbeGuideConfig[IO]): IO[Option[NodChopTrackingConfig]] = (
    for {
      aa <-  OptionT.liftF(g.nodachopa.map(decodeNodChopOption))
      ab <-  OptionT.liftF(g.nodachopb.map(decodeNodChopOption))
      ac <-  OptionT.liftF(g.nodachopc.map(decodeNodChopOption))
      ba <-  OptionT.liftF(g.nodbchopa.map(decodeNodChopOption))
      bb <-  OptionT.liftF(g.nodbchopb.map(decodeNodChopOption))
      bc <-  OptionT.liftF(g.nodbchopc.map(decodeNodChopOption))
      ca <-  OptionT.liftF(g.nodcchopa.map(decodeNodChopOption))
      cb <-  OptionT.liftF(g.nodcchopb.map(decodeNodChopOption))
      cc <-  OptionT.liftF(g.nodcchopc.map(decodeNodChopOption))

      // This last product is slightly tricky.
      o  <- OptionT(IO{
        if (List(aa, ab, ac, ba, bb, bc, ca, cb, cc).contains(true)) {
          if (List(aa, bb).forall(_ === true) && List(ab, ac, ba, bc, ca, cb, cc).forall(_ === false)) {
            Some(NodChopTrackingConfig.Normal)
          } else {
            List(
              (aa, NodChop(Beam.A, Beam.A)), (ab, NodChop(Beam.A, Beam.B)), (ac, NodChop(Beam.A, Beam.C)),
              (ba, NodChop(Beam.B, Beam.A)), (bb, NodChop(Beam.B, Beam.B)), (bc, NodChop(Beam.B, Beam.C)),
              (ca, NodChop(Beam.C, Beam.A)), (cb, NodChop(Beam.C, Beam.B)), (cc, NodChop(Beam.C, Beam.C))
            ) collect {
              case (true, a) => a
            } match {
              case h :: t => Some(NodChopTrackingConfig.Special(OneAnd(h, t)))
              case Nil    => None // the list is empty
            }
          }
        } else Some(NodChopTrackingConfig.AllOff)
      })
    } yield o
  ).value

  private def calcProbeTrackingConfig(f: FollowOption, t: NodChopTrackingConfig): ProbeTrackingConfig = (f, t) match {
    case (FollowOff, _)                            => ProbeTrackingConfig.Off
    case (FollowOn, NodChopTrackingConfig.AllOff)  => ProbeTrackingConfig.Frozen
    case (FollowOn, v:ActiveNodChopTracking)       => ProbeTrackingConfig.On(v)
  }

  implicit private val decodeFollowOption: DecodeEpicsValue[String, FollowOption] = DecodeEpicsValue((s: String)
  => if (s.trim === "Off") FollowOff else FollowOn)

  implicit private val decodeGuideSensorOption: DecodeEpicsValue[BinaryYesNo, GuiderSensorOption] =
    DecodeEpicsValue((s: BinaryYesNo) => if (s === BinaryYesNo.No) GuiderSensorOff else GuiderSensorOn)

  private def getPwfs1: IO[GuiderConfig] = for {
    prk <- getStatusVal(TcsEpics.instance.p1Parked, "PWFS1 parked state").recover{case _ => false}
    trk <- getStatusVal(getNodChopTrackingConfig(TcsEpics.instance.pwfs1ProbeGuideConfig), "PWFS1 tracking configuration")
    fol <- getStatusVal(TcsEpics.instance.p1FollowS, "PWFS1 follow state").map(decode[String, FollowOption])
    wfs <- getStatusVal(TcsEpics.instance.pwfs1On, "PWFS1 detector").map(decode[BinaryYesNo, GuiderSensorOption])
  } yield GuiderConfig(prk.fold(ProbeTrackingConfig.Parked, calcProbeTrackingConfig(fol, trk)), wfs)

  // P2 probe guide configuration is partially shared with Altair guide configuration. useAo tells which one is.
  // TODO: Make sure it works for GS.
  private def getPwfs2OrAowfs(getAoFollow: IO[Option[Boolean]]): IO[Either[GuiderConfig, ProbeTrackingConfig]] =
    for {
      useAo <- getStatusVal(TcsEpics.instance.useAo, "use AO flag")
      aoFol <- getStatusVal(getAoFollow, "AO follow state").map(_.fold(FollowOn, FollowOff))
      prk   <- getStatusVal(TcsEpics.instance.p2Parked, "PWFS2 parked state")
      trk   <- getStatusVal(getNodChopTrackingConfig(TcsEpics.instance.pwfs2ProbeGuideConfig), "PWFS2 tracking configuration")
      fol   <- getStatusVal(TcsEpics.instance.p2FollowS.map(decode[String, FollowOption]), "PWFS2 follow state")
      wfs   <- getStatusVal(TcsEpics.instance.pwfs2On.map(decode[BinaryYesNo, GuiderSensorOption]), "PWFS2 detector")
    } yield if(useAo === BinaryYesNo.Yes) Right(calcProbeTrackingConfig(aoFol, trk))
            else Left(GuiderConfig(prk.fold(ProbeTrackingConfig.Parked, calcProbeTrackingConfig(fol, trk)), wfs))

  private def getOiwfs: IO[GuiderConfig] = for {
    prk <- getStatusVal(TcsEpics.instance.oiParked, "OIWFS parked state")
    trk <- getStatusVal(getNodChopTrackingConfig(TcsEpics.instance.oiwfsProbeGuideConfig), "OIWFS tracking configuration")
    fol <- getStatusVal(TcsEpics.instance.oiFollowS.map(decode[String, FollowOption]), "OIWFS follow state")
    wfs <- getStatusVal(TcsEpics.instance.oiwfsOn.map(decode[BinaryYesNo, GuiderSensorOption]), "OIWFS detector")
  } yield GuiderConfig(prk.fold(ProbeTrackingConfig.Parked, calcProbeTrackingConfig(fol, trk)), wfs)

  import ScienceFoldPositionCodex._

  private def getScienceFoldPosition: IO[Option[ScienceFold]] =
    for {
      sfPos    <- getStatusVal(TcsEpics.instance.sfName,"SF position")
      sfParked <- getStatusVal(TcsEpics.instance.sfParked.map(_ =!= 0), "SF park")
    } yield if (sfParked) ScienceFold.Parked.some
            else decode[String, Option[ScienceFold]](sfPos)

  implicit val decodeHwrsPickupPosition: DecodeEpicsValue[String, HrwfsPickupPosition] = DecodeEpicsValue((t: String)
  => if (t.trim === "IN") HrwfsPickupPosition.IN
    else HrwfsPickupPosition.OUT)

  private def getHrwfsPickupPosition: IO[HrwfsPickupPosition] =
    for {
      hwPos <- getStatusVal(TcsEpics.instance.agHwName, "Pickup position").map(decode[String, HrwfsPickupPosition])
      hwParked <- getStatusVal(TcsEpics.instance.agHwParked, "Pickup park").map(_ =!= 0)
    } yield if (hwParked) HrwfsPickupPosition.Parked
      else hwPos

  private def getStatusVal[F[_]: MonadError[?[_], Throwable], A](get: F[A], name: String): F[A] =
    get.adaptError {
      case _ => SeqexecFailure.Unexpected(s"Unable to read $name from TCS.")
    }

  private def getIAA: IO[Angle] = getStatusVal(TcsEpics.instance.instrAA.map(Degrees(_)), "IAA")

  private def getOffsetX: IO[Length] = getStatusVal(TcsEpics.instance.xoffsetPoA1.map(Millimeters(_)),
    "X offset")

  private def getOffsetY: IO[Length] = getStatusVal(TcsEpics.instance.yoffsetPoA1.map(Millimeters(_)),
    "Y offset")

  private def getWavelength: IO[Wavelength] =
    getStatusVal(TcsEpics.instance.sourceAWavelength.map(v => Wavelength(Angstroms(v))), "central wavelength")

  private def getInstrumentPorts: IO[InstrumentPorts] =
    for {
      f2    <- TcsEpics.instance.f2Port.handleError(_ => InvalidPort)
      ghost <- TcsEpics.instance.ghostPort.handleError(_ => InvalidPort)
      gmos  <- TcsEpics.instance.gmosPort.handleError(_ => InvalidPort)
      gnirs <- TcsEpics.instance.gnirsPort.handleError(_ => InvalidPort)
      gpi   <- TcsEpics.instance.gpiPort.handleError(_ => InvalidPort)
      gsaoi <- TcsEpics.instance.gsaoiPort.handleError(_ => InvalidPort)
      nifs  <- TcsEpics.instance.nifsPort.handleError(_ => InvalidPort)
      niri  <- TcsEpics.instance.niriPort.handleError(_ => InvalidPort)
    } yield InstrumentPorts(
        f2,
        ghost,
        gmos,
        gnirs,
        gpi,
        gsaoi,
        nifs,
        niri
      )

  def retrieveConfiguration(getAoFollow: IO[Option[Boolean]]): IO[EpicsTcsConfig] =
    for {
      iaa    <- getIAA
      offX   <- getOffsetX
      offY   <- getOffsetY
      wl     <- getWavelength
      p1     <- getPwfs1
      p2OrAo <- getPwfs2OrAowfs(getAoFollow)
      oi     <- getOiwfs
      tgc    <- getGuideConfig
      aof    <- getAoFold
      sf     <- getScienceFoldPosition
      hr     <- getHrwfsPickupPosition
      ports  <- getInstrumentPorts
    } yield EpicsTcsConfig(
      iaa,
      FocalPlaneOffset(tag[OffsetX](offX), tag[OffsetY](offY)),
      wl,
      p1,
      p2OrAo,
      oi,
      tgc,
      aof,
      sf,
      hr,
      ports
    )

}
