// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server.tcs

import cats.effect.{IO, Sync}
import cats.implicits._
import edu.gemini.epics.acm._
import edu.gemini.seqexec.server.tcs.{BinaryOnOff, BinaryYesNo}
import org.log4s.{Logger, getLogger}
import seqexec.server.EpicsCommand._
import seqexec.server.EpicsUtil._
import seqexec.server.{EpicsCommand, EpicsSystem, SeqAction}
import squants.Angle
import squants.Time
import squants.space.Degrees
import squants.time.TimeConversions._

/**
 * TcsEpics wraps the non-functional parts of the EPICS ACM library to interact with TCS. It has all the objects used
 * to read TCS status values and execute TCS commands.
 *
 * Created by jluhrs on 10/1/15.
 */

// scalastyle:off
final class TcsEpics[F[_]: Sync](epicsService: CaService, tops: Map[String, String]) {

  import EpicsCommand.setParameter
  import TcsEpics._

  val TcsTop: String = tops.getOrElse("tcs", "")

  // This is a bit ugly. Commands are triggered from the main apply record, so I just choose an arbitrary command here.
  // Triggering that command will trigger all the marked commands.
  def post: SeqAction[EpicsCommand.Result] = m1GuideCmd.post

  object m1GuideCmd extends EpicsCommand {
    override val cs: Option[CaCommandSender] = Option(epicsService.getCommandSender("m1Guide"))
    private val state = cs.map(_.getString("state"))

    def setState(v: String): SeqAction[Unit] = setParameter(state, v)
  }

  object m2GuideCmd extends EpicsCommand {
    override val cs: Option[CaCommandSender] = Option(epicsService.getCommandSender("m2Guide"))
    private val state = cs.map(_.getString("state"))

    def setState(v: String): SeqAction[Unit] = setParameter[String](state, v)
  }

  object m2GuideModeCmd extends EpicsCommand {
    override protected val cs: Option[CaCommandSender] = Option(epicsService.getCommandSender("m2GuideMode"))

    private val coma = cs.map(_.getString("coma"))
    def setComa(v: String): SeqAction[Unit] = setParameter[String](coma, v)
  }

  object m2GuideConfigCmd extends EpicsCommand {
    override protected val cs: Option[CaCommandSender] = Option(epicsService.getCommandSender("m2GuideConfig"))

    private val source = cs.map(_.getString("source"))
    def setSource(v: String): SeqAction[Unit] = setParameter[String](source, v)

    private val beam = cs.map(_.getString("beam"))
    def setBeam(v: String): SeqAction[Unit] = setParameter[String](beam, v)

    private val reset = cs.map(_.getString("reset"))
    def setReset(v: String): SeqAction[Unit] = setParameter[String](reset, v)
  }

  object mountGuideCmd extends EpicsCommand {
    override val cs: Option[CaCommandSender] = Option(epicsService.getCommandSender("mountGuide"))

    private val source = cs.map(_.getString("source"))

    def setSource(v: String): SeqAction[Unit] = setParameter(source, v)

    private val p1weight = cs.map(_.getDouble("p1weight"))

    def setP1Weight(v: Double): SeqAction[Unit] = setParameter[java.lang.Double](p1weight, v)

    private val p2weight = cs.map(_.getDouble("p2weight"))

    def setP2Weight(v: Double): SeqAction[Unit] = setParameter[java.lang.Double](p2weight, v)

    private val mode = cs.map(_.getString("mode"))

    def setMode(v: String): SeqAction[Unit] = setParameter(mode, v)
  }

  object offsetACmd extends EpicsCommand {
    override val cs: Option[CaCommandSender] = Option(epicsService.getCommandSender("offsetPoA1"))

    private val x = cs.map(_.getDouble("x"))

    def setX(v: Double): SeqAction[Unit] = setParameter[java.lang.Double](x, v)

    private val y = cs.map(_.getDouble("y"))

    def setY(v: Double): SeqAction[Unit] = setParameter[java.lang.Double](y, v)
  }

  object offsetBCmd extends EpicsCommand {
    override val cs: Option[CaCommandSender] = Option(epicsService.getCommandSender("offsetPoB1"))

    private val x = cs.map(_.getDouble("x"))

    def setX(v: Double): SeqAction[Unit] = setParameter[java.lang.Double](x, v)

    private val y = cs.map(_.getDouble("y"))

    def setY(v: Double): SeqAction[Unit] = setParameter[java.lang.Double](y, v)
  }

  object offsetCCmd extends EpicsCommand {
    override val cs: Option[CaCommandSender] = Option(epicsService.getCommandSender("offsetPoC1"))

    private val x = cs.map(_.getDouble("x"))

    def setX(v: Double): SeqAction[Unit] = setParameter[java.lang.Double](x, v)

    private val y = cs.map(_.getDouble("y"))

    def setY(v: Double): SeqAction[Unit] = setParameter[java.lang.Double](y, v)
  }

  object wavelSourceA extends EpicsCommand {
    override val cs: Option[CaCommandSender] = Option(epicsService.getCommandSender("wavelSourceA"))

    private val wavel = cs.map(_.getDouble("wavel"))

    def setWavel(v: Double): SeqAction[Unit] = setParameter[java.lang.Double](wavel, v)
  }

  object wavelSourceB extends EpicsCommand {
    override val cs: Option[CaCommandSender] = Option(epicsService.getCommandSender("wavelSourceB"))

    private val wavel = cs.map(_.getDouble("wavel"))

    def setWavel(v: Double): SeqAction[Unit] = setParameter[java.lang.Double](wavel, v)
  }

  object wavelSourceC extends EpicsCommand {
    override val cs: Option[CaCommandSender] = Option(epicsService.getCommandSender("wavelSourceC"))

    private val wavel = cs.map(_.getDouble("wavel"))

    def setWavel(v: Double): SeqAction[Unit] = setParameter[java.lang.Double](wavel, v)
  }

  object m2Beam extends EpicsCommand {
    override val cs: Option[CaCommandSender] = Option(epicsService.getCommandSender("m2Beam"))

    private val beam = cs.map(_.getString("beam"))

    def setBeam(v: String): SeqAction[Unit] = setParameter(beam, v)
  }

  val pwfs1ProbeGuideCmd: ProbeGuideCmd = new ProbeGuideCmd("pwfs1Guide", epicsService)

  val pwfs2ProbeGuideCmd: ProbeGuideCmd = new ProbeGuideCmd("pwfs2Guide", epicsService)

  val oiwfsProbeGuideCmd: ProbeGuideCmd = new ProbeGuideCmd("oiwfsGuide", epicsService)

  val pwfs1ProbeFollowCmd: ProbeFollowCmd = new ProbeFollowCmd("p1Follow", epicsService)

  val pwfs2ProbeFollowCmd: ProbeFollowCmd = new ProbeFollowCmd("p2Follow", epicsService)

  val oiwfsProbeFollowCmd: ProbeFollowCmd = new ProbeFollowCmd("oiFollow", epicsService)

  val aoProbeFollowCmd: ProbeFollowCmd = new ProbeFollowCmd("aoFollow", epicsService)

  object pwfs1Park extends EpicsCommand {
    override val cs: Option[CaCommandSender] = Option(epicsService.getCommandSender("pwfs1Park"))
  }

  object pwfs2Park extends EpicsCommand {
    override val cs: Option[CaCommandSender] = Option(epicsService.getCommandSender("pwfs2Park"))
  }

  object oiwfsPark extends EpicsCommand {
    override val cs: Option[CaCommandSender] = Option(epicsService.getCommandSender("oiwfsPark"))
  }

  object pwfs1StopObserveCmd extends EpicsCommand {
    override val cs: Option[CaCommandSender] = Option(epicsService.getCommandSender("pwfs1StopObserve"))
  }

  object pwfs2StopObserveCmd extends EpicsCommand {
    override val cs: Option[CaCommandSender] = Option(epicsService.getCommandSender("pwfs2StopObserve"))
  }

  object oiwfsStopObserveCmd extends EpicsCommand {
    override val cs: Option[CaCommandSender] = Option(epicsService.getCommandSender("oiwfsStopObserve"))
  }

  val pwfs1ObserveCmd: WfsObserveCmd = new WfsObserveCmd("pwfs1Observe", epicsService)

  val pwfs2ObserveCmd: WfsObserveCmd = new WfsObserveCmd("pwfs2Observe", epicsService)

  val oiwfsObserveCmd: WfsObserveCmd = new WfsObserveCmd("oiwfsObserve", epicsService)

  object hrwfsParkCmd extends EpicsCommand {
    override val cs: Option[CaCommandSender] = Option(epicsService.getCommandSender("hrwfsPark"))
  }

  object hrwfsPosCmd extends EpicsCommand {
    override val cs: Option[CaCommandSender] = Option(epicsService.getCommandSender("hrwfs"))

    private val hrwfsPos = cs.map(_.getString("hrwfsPos"))

    def setHrwfsPos(v: String): SeqAction[Unit] = setParameter(hrwfsPos, v)
  }

  object scienceFoldParkCmd extends EpicsCommand {
    override val cs: Option[CaCommandSender] = Option(epicsService.getCommandSender("scienceFoldPark"))
  }

  object scienceFoldPosCmd extends EpicsCommand {
    override val cs: Option[CaCommandSender] = Option(epicsService.getCommandSender("scienceFold"))

    private val scfold = cs.map(_.getString("scfold"))

    def setScfold(v: String): SeqAction[Unit] = setParameter(scfold, v)
  }

  object observe extends EpicsCommand {
    override val cs: Option[CaCommandSender] = Option(epicsService.getCommandSender("tcs::observe"))
  }

  object endObserve extends EpicsCommand {
    override val cs: Option[CaCommandSender] = Option(epicsService.getCommandSender("tcs::endObserve"))
  }

  object aoCorrect extends EpicsCommand {
    override protected val cs: Option[CaCommandSender] = Option(epicsService.getCommandSender("aoCorrect"))

    private val correct = cs.map(_.getString("correct"))
    def setCorrections(v: String): SeqAction[Unit] = setParameter(correct, v)

    private val gains = cs.map(_.getInteger("gains"))
    def setGains(v: Int): SeqAction[Unit] = setParameter[java.lang.Integer](gains, v)

    private val matrix = cs.map(_.getInteger("matrix"))
    def setMatrix(v: Int): SeqAction[Unit] = setParameter[java.lang.Integer](matrix, v)
  }

  object aoPrepareControlMatrix extends EpicsCommand {
    override protected val cs: Option[CaCommandSender] = Option(epicsService.getCommandSender("aoPrepareCm"))

    private val x = cs.map(_.getDouble("x"))
    def setX(v: Double): SeqAction[Unit] = setParameter[java.lang.Double](x, v)

    private val y = cs.map(_.getDouble("y"))
    def setY(v: Double): SeqAction[Unit] = setParameter[java.lang.Double](y, v)

    private val seeing = cs.map(_.getDouble("seeing"))
    def setSeeing(v: Double): SeqAction[Unit] = setParameter[java.lang.Double](seeing, v)

    private val starMagnitude = cs.map(_.getDouble("gsmag"))
    def setStarMagnitude(v: Double): SeqAction[Unit] = setParameter[java.lang.Double](starMagnitude, v)

    private val windSpeed = cs.map(_.getDouble("windspeed"))
    def setWindSpeed(v: Double): SeqAction[Unit] = setParameter[java.lang.Double](windSpeed, v)
  }

  object aoFlatten extends EpicsCommand {
    override protected val cs: Option[CaCommandSender] = Option(epicsService.getCommandSender("aoFlatten"))
  }

  object aoStatistics extends EpicsCommand {
    override protected val cs: Option[CaCommandSender] = Option(epicsService.getCommandSender("aoStats"))

    private val fileName = cs.map(_.getString("filename"))
    def setFileName(v: String): SeqAction[Unit] = setParameter(fileName, v)

    private val samples = cs.map(_.getInteger("samples"))
    def setSamples(v: Int): SeqAction[Unit] = setParameter[java.lang.Integer](samples, v)

    private val interval = cs.map(_.getDouble("interval"))
    def setInterval(v: Double): SeqAction[Unit] = setParameter[java.lang.Double](interval, v)

    private val triggerTime = cs.map(_.getDouble("trigtime"))
    def setTriggerTimeInterval(v: Double): SeqAction[Unit] = setParameter[java.lang.Double](triggerTime, v)
  }

  object targetFilter extends EpicsCommand {
    override protected val cs: Option[CaCommandSender] = Option(epicsService.getCommandSender("filter1"))

    private val bandwidth = cs.map(_.getDouble("bandwidth"))
    def setBandwidth(v: Double): SeqAction[Unit] = setParameter[java.lang.Double](bandwidth, v)

    private val maxVelocity = cs.map(_.getDouble("maxv"))
    def setMaxVelocity(v: Double): SeqAction[Unit] = setParameter[java.lang.Double](maxVelocity, v)

    private val grabRadius = cs.map(_.getDouble("grab"))
    def setGrabRadius(v: Double): SeqAction[Unit] = setParameter[java.lang.Double](grabRadius, v)

    private val shortCircuit = cs.map(_.getString("shortCircuit"))
    def setShortCircuit(v: String): SeqAction[Unit] = setParameter(shortCircuit, v)
  }

  private val tcsState = epicsService.getStatusAcceptor("tcsstate")

  private[tcs] def readD(name: String): F[Double] =
    safeAttributeSDoubleF[F](name, tcsState.getDoubleAttribute(name))
  private[tcs] def read(name: String): F[String] =
    safeAttributeF[F, String](name, tcsState.getStringAttribute(name))
  private[tcs] def readI(name: String): F[Int] =
    safeAttributeSIntF[F](name, tcsState.getIntegerAttribute(name))

  def absorbTipTilt: F[Int] = readI("absorbTipTilt")

  def m1GuideSource: F[String] = read("m1GuideSource")

  private val m1GuideAttr: CaAttribute[BinaryOnOff] = tcsState.addEnum("m1Guide",
    s"${TcsTop}im:m1GuideOn", classOf[BinaryOnOff], "M1 guide")
  def m1Guide: F[BinaryOnOff] = safeAttributeF("m1Guide", m1GuideAttr)

  def m2p1Guide: F[String] = read("m2p1Guide")

  def m2p2Guide: F[String] = read("m2p2Guide")

  def m2oiGuide: F[String] = read("m2oiGuide")

  def m2aoGuide: F[String] = read("m2aoGuide")

  def comaCorrect: F[String] = read("comaCorrect")

  private val m2GuideStateAttr: CaAttribute[BinaryOnOff] = tcsState.addEnum("m2GuideState",
    s"${TcsTop}om:m2GuideState", classOf[BinaryOnOff], "M2 guiding state")
  def m2GuideState: F[BinaryOnOff] = safeAttributeF("m2GuideState", m2GuideStateAttr)

  def xoffsetPoA1: F[Double] = readD("xoffsetPoA1")

  def yoffsetPoA1: F[Double] = readD("yoffsetPoA1")

  def xoffsetPoB1: F[Double] = readD("xoffsetPoB1")

  def yoffsetPoB1: F[Double] = readD("yoffsetPoB1")

  def xoffsetPoC1: F[Double] = readD("xoffsetPoC1")

  def yoffsetPoC1: F[Double] = readD("yoffsetPoC1")

  def sourceAWavelength: F[Double] = readD("sourceAWavelength")

  def sourceBWavelength: F[Double] = readD("sourceBWavelength")

  def sourceCWavelength: F[Double] = readD("sourceCWavelength")

  def chopBeam: F[String] = read("chopBeam")

  def p1FollowS: F[String] = read("p1FollowS")

  def p2FollowS: F[String] = read("p2FollowS")

  def oiFollowS: F[String] = read("oiFollowS")

  def aoFollowS: F[String] = read("aoFollowS")

  def p1Parked: F[Boolean] = readI("p1Parked").map(_ =!= 0)

  def p2Parked: F[Boolean] = readI("p2Parked").map(_ =!= 0)

  def oiParked: F[Boolean] = readI("oiParked").map(_ =!= 0)

  private val pwfs1OnAttr: CaAttribute[BinaryYesNo] = tcsState.addEnum("pwfs1On",
    s"${TcsTop}drives:p1Integrating", classOf[BinaryYesNo], "P1 integrating")
  def pwfs1On: F[BinaryYesNo] = safeAttributeF("pwfs1On", pwfs1OnAttr)

  private val pwfs2OnAttr: CaAttribute[BinaryYesNo] = tcsState.addEnum("pwfs2On",
    s"${TcsTop}drives:p2Integrating", classOf[BinaryYesNo], "P2 integrating")

  def pwfs2On:F[BinaryYesNo] = safeAttributeF("pwfs2On", pwfs2OnAttr)

  private val oiwfsOnAttr: CaAttribute[BinaryYesNo] = tcsState.addEnum("oiwfsOn",
    s"${TcsTop}drives:oiIntegrating", classOf[BinaryYesNo], "P2 integrating")

  def oiwfsOn: F[BinaryYesNo] = safeAttributeF("oiwfsOn", oiwfsOnAttr)

  def sfName: F[String] = read("sfName")

  def sfParked: F[Int] = readI("sfParked")

  def agHwName: F[String] = read("agHwName")

  def agHwParked: F[Int] = readI("agHwParked")

  def instrAA: F[Double] = readD("instrAA")

  private val inPositionAttr: CaAttribute[String] = tcsState.getStringAttribute("inPosition")

  def inPosition:F[String] = safeAttributeF("inPosition", inPositionAttr)

  private val agInPositionAttr: CaAttribute[java.lang.Double] = tcsState.getDoubleAttribute("agInPosition")
  def agInPosition:F[Double] = readD("agInPosition")

  val pwfs1ProbeGuideConfig: ProbeGuideConfig[F] = new ProbeGuideConfig("p1", tcsState)

  val pwfs2ProbeGuideConfig: ProbeGuideConfig[F] = new ProbeGuideConfig("p2", tcsState)

  val oiwfsProbeGuideConfig: ProbeGuideConfig[F] = new ProbeGuideConfig("oi", tcsState)

  private val tcsStabilizeTime = 1.seconds

  private val filteredInPositionAttr: CaWindowStabilizer[String] = new CaWindowStabilizer[String](inPositionAttr, java.time.Duration.ofMillis(tcsStabilizeTime.toMillis))
  def filteredInPosition:F[String] = safeAttributeF("filteredInPositionAttr", filteredInPositionAttr)

  // This functions returns a SeqAction that, when run, will wait up to `timeout`
  // seconds for the TCS in-position flag to set to TRUE
  def waitInPosition(timeout: Time): SeqAction[Unit] = SeqAction(filteredInPositionAttr.reset)
    .flatMap(waitForValue(_, "TRUE", timeout,"TCS inposition flag"))

  private val agStabilizeTime = 1.seconds

  private val filteredAGInPositionAttr: CaWindowStabilizer[java.lang.Double] = new CaWindowStabilizer[java.lang.Double](agInPositionAttr, java.time.Duration.ofMillis(agStabilizeTime.toMillis))
  def filteredAGInPosition: F[Double] =
    safeAttributeSDoubleF[F]("filteredAGInPosition", filteredAGInPositionAttr )

  // `waitAGInPosition` works like `waitInPosition`, but for the AG in-position flag.
  /* TODO: AG inposition can take up to 1[s] to react to a TCS command. If the value is read before that, it may induce
   * an error. A better solution is to detect the edge, from not in position to in-position.
   */
  private val AGSettleTime = 1100.milliseconds
  def waitAGInPosition(timeout: Time): SeqAction[Unit] = SeqAction(Thread.sleep(AGSettleTime.toMilliseconds.toLong)) *>
    SeqAction(filteredAGInPositionAttr.reset).flatMap(
      waitForValue[java.lang.Double](_, 1.0, timeout, "AG inposition flag"))

  def hourAngle: F[String] = read("ha")

  def localTime: F[String] = read("lt")

  def trackingFrame: F[String] = read("trkframe")

  def trackingEpoch: F[Double] = readD("trkepoch")

  def equinox: F[Double] = readD("sourceAEquinox")

  def trackingEquinox: F[String] = read("sourceATrackEq")

  def trackingDec: F[Double] = readD("dectrack")

  def trackingRA: F[Double] = readD("ratrack")

  def elevation: F[Double] = readD("elevatio")

  def azimuth: F[Double] = readD("azimuth")

  def crPositionAngle: F[Double] = readD("crpa")

  def ut: F[String] = read("ut")

  def date: F[String] = read("date")

  def m2Baffle: F[String] = read("m2baffle")

  def m2CentralBaffle: F[String] = read("m2cenbaff")

  def st: F[String] = read("st")

  def sfRotation: F[Double] = readD("sfrt2")

  def sfTilt: F[Double] = readD("sftilt")

  def sfLinear: F[Double] = readD("sflinear")

  def instrPA: F[Double] = readD("instrPA")

  def targetA: F[List[Double]] = safeAttributeSListSDoubleF("targetA", tcsState.getDoubleAttribute("targetA"))

  def aoFoldPosition: F[String] = read("aoName")

  private val useAoAttr: CaAttribute[BinaryYesNo] = tcsState.addEnum("useAo",
    s"${TcsTop}im:AOConfigFlag.VAL", classOf[BinaryYesNo], "Using AO flag")
  def useAo: F[BinaryYesNo] = safeAttributeF("useAo", useAoAttr)

  def airmass: F[Double] = readD("airmass")

  def airmassStart: F[Double] = readD("amstart")

  def airmassEnd: F[Double] = readD("amend")

  def carouselMode: F[String] = read("cguidmod")

  def crFollow: F[Int]  = readI("crfollow")

  def sourceATarget: Target[F] = new Target[F] {
    override def epoch: F[String] = read("sourceAEpoch")

    override def equinox: F[String] = read("sourceAEquinox")

    override def radialVelocity:F[Double] = readD("radvel")

    override def frame: F[String] = read("frame")

    override def centralWavelenght: F[Double] = sourceAWavelength

    override def ra: F[Double] = readD("ra")

    override def objectName: F[String] = read("sourceAObjectName")

    override def dec: F[Double] = readD("dec")

    override def parallax: F[Double] = readD("parallax")

    override def properMotionRA: F[Double] = readD("pmra")

    override def properMotionDec: F[Double] = readD("pmdec")
  }

  private def target(base: String): Target[F] = new Target[F] {
      override def epoch: F[String] = read(base + "aepoch")
      override def equinox: F[String] = read(base + "aequin")
      override def radialVelocity:F[Double] = readD(base + "arv")
      override def frame: F[String]  = read(base + "aframe")
      override def centralWavelenght:F[Double] =
        readD(base + "awavel")
      override def ra:F[Double] = readD(base + "ara")
      override def objectName: F[String] = read(base + "aobjec")
      override def dec:F[Double] = readD(base + "adec")
      override def parallax:F[Double] = readD(base + "aparal")
      override def properMotionRA:F[Double] = readD(base + "apmra")
      override def properMotionDec:F[Double] =
        readD(base + "apmdec")
    }

  def pwfs1Target: Target[F] = target("p1")

  def pwfs2Target: Target[F] = target("p2")

  def oiwfsTarget: Target[F] = target("oi")

  def gwfs1Target: Target[F] = target("g1")

  def gwfs2Target: Target[F] = target("g2")

  def gwfs3Target: Target[F] = target("g3")

  def gwfs4Target: Target[F] = target("g4")

  def parallacticAngle: F[Angle] =
    readD("parangle").map(Degrees(_))

  def m2UserFocusOffset: F[Double] = readD("m2ZUserOffset")
  private val pwfs1Status = epicsService.getStatusAcceptor("pwfs1state")

  def pwfs1IntegrationTime: F[Double] = safeAttributeSDoubleF("pwfs1state:intTime", pwfs1Status.getDoubleAttribute("intTime"))

  private val pwfs2Status = epicsService.getStatusAcceptor("pwfs2state")

  def pwfs2IntegrationTime: F[Double] = safeAttributeSDoubleF("pwfs2state:intTime", pwfs2Status.getDoubleAttribute("intTime"))

  private val oiwfsStatus = epicsService.getStatusAcceptor("oiwfsstate")

  // Attribute must be changed back to Double after EPICS channel is fixed.
  def oiwfsIntegrationTime:F[Double]  = safeAttributeSDoubleF("oiwfsstate:intTime", oiwfsStatus.getDoubleAttribute("intTime"))

  private def instPort(name: String): F[Int] =
    readI(s"${name}Port")

  def gsaoiPort: F[Int] = instPort("gsaoi")
  def gpiPort: F[Int]= instPort("gpi")
  def f2Port: F[Int] = instPort("f2")
  def niriPort: F[Int] = instPort("niri")
  def gnirsPort: F[Int] = instPort("nirs")
  def nifsPort: F[Int] = instPort("nifs")
  def gmosPort: F[Int] = instPort("gmos")
  def ghostPort: F[Int] = instPort("ghost")

  def aoGuideStarX: F[Double] = readD("aogsx")

  def aoGuideStarY: F[Double] = readD("aogsy")

  def aoPreparedCMX: F[Double] = read("cmprepx")
    .map(_.toDouble)

  def aoPreparedCMY: F[Double] = read("cmprepy")
    .map(_.toDouble)
}

object TcsEpics extends EpicsSystem[TcsEpics[IO]] {

  override val className: String = getClass.getName
  override val Log: Logger = getLogger
  override val CA_CONFIG_FILE: String = "/Tcs.xml"

  override def build(service: CaService, tops: Map[String, String]) = new TcsEpics(service, tops)

  sealed class ProbeGuideCmd(csName: String, epicsService: CaService) extends EpicsCommand {
    override val cs: Option[CaCommandSender] = Option(epicsService.getCommandSender(csName))

    private val nodachopa = cs.map(_.getString("nodachopa"))
    def setNodachopa(v: String): SeqAction[Unit] = setParameter(nodachopa, v)

    private val nodachopb = cs.map(_.getString("nodachopb"))
    def setNodachopb(v: String): SeqAction[Unit] = setParameter(nodachopb, v)

    private val nodachopc = cs.map(_.getString("nodachopc"))
    def setNodachopc(v: String): SeqAction[Unit] = setParameter(nodachopc, v)

    private val nodbchopa = cs.map(_.getString("nodbchopa"))
    def setNodbchopa(v: String): SeqAction[Unit] = setParameter(nodbchopa, v)

    private val nodbchopb = cs.map(_.getString("nodbchopb"))
    def setNodbchopb(v: String): SeqAction[Unit] = setParameter(nodbchopb, v)

    private val nodbchopc = cs.map(_.getString("nodbchopc"))
    def setNodbchopc(v: String): SeqAction[Unit] = setParameter(nodbchopc, v)

    private val nodcchopa = cs.map(_.getString("nodcchopa"))
    def setNodcchopa(v: String): SeqAction[Unit] = setParameter(nodcchopa, v)

    private val nodcchopb = cs.map(_.getString("nodcchopb"))
    def setNodcchopb(v: String): SeqAction[Unit] = setParameter(nodcchopb, v)

    private val nodcchopc = cs.map(_.getString("nodcchopc"))
    def setNodcchopc(v: String): SeqAction[Unit] = setParameter(nodcchopc, v)
  }

  sealed class WfsObserveCmd(csName: String, epicsService: CaService) extends EpicsCommand {
    override val cs: Option[CaCommandSender] = Option(epicsService.getCommandSender(csName))

    private val noexp = cs.map(_.getInteger("noexp"))
    def setNoexp(v: Integer): SeqAction[Unit] = setParameter(noexp, v)

    private val int = cs.map(_.getDouble("int"))
    def setInt(v: Double): SeqAction[Unit] = setParameter[java.lang.Double](int, v)

    private val outopt = cs.map(_.getString("outopt"))
    def setOutopt(v: String): SeqAction[Unit] = setParameter(outopt, v)

    private val label = cs.map(_.getString("label"))
    def setLabel(v: String): SeqAction[Unit] = setParameter(label, v)

    private val output = cs.map(_.getString("output"))
    def setOutput(v: String): SeqAction[Unit] = setParameter(output, v)

    private val path = cs.map(_.getString("path"))
    def setPath(v: String): SeqAction[Unit] = setParameter(path, v)

    private val name = cs.map(_.getString("name"))
    def setName(v: String): SeqAction[Unit] = setParameter(name, v)
  }

  final class ProbeFollowCmd(csName: String, epicsService: CaService) extends EpicsCommand {
    override protected val cs: Option[CaCommandSender] = Option(epicsService.getCommandSender(csName))

    private val follow = cs.map(_.getString("followState"))
    def setFollowState(v: String): SeqAction[Unit] = setParameter(follow, v)
  }

  class ProbeGuideConfig[F[_]: Sync](protected val prefix: String, protected val tcsState: CaStatusAcceptor) {
    private def read(name: String): F[String] =
    safeAttributeF[F, String](name, tcsState.getStringAttribute(name))
    def nodachopa: F[String] = read(s"${prefix}nodachopa")
    def nodachopb: F[String] = read(s"${prefix}nodachopb")
    def nodachopc: F[String] = read(s"${prefix}nodachopc")
    def nodbchopa: F[String] = read(s"${prefix}nodbchopa")
    def nodbchopb: F[String] = read(s"${prefix}nodbchopb")
    def nodbchopc: F[String] = read(s"${prefix}nodbchopc")
    def nodcchopa: F[String] = read(s"${prefix}nodcchopa")
    def nodcchopb: F[String] = read(s"${prefix}nodcchopb")
    def nodcchopc: F[String] = read(s"${prefix}nodcchopc")
  }

  sealed trait Target[F[_]] {
    def objectName: F[String]
    def ra: F[Double]
    def dec: F[Double]
    def frame: F[String]
    def equinox: F[String]
    def epoch: F[String]
    def properMotionRA: F[Double]
    def properMotionDec: F[Double]
    def centralWavelenght: F[Double]
    def parallax: F[Double]
    def radialVelocity: F[Double]
  }

}
// scalastyle:on
