// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.seqexec.model

import monocle.{Lens, Optional, Prism, Traversal}
import monocle.macros.{GenLens, GenPrism, Lenses}
import monocle.function.At.atMap
import monocle.function.At.at
import monocle.std.option.some
import monocle.Iso

import scalaz.{Applicative, Equal, Show, Order, NonEmptyList}
import scalaz.std.list._
import scalaz.std.anyVal._
import scalaz.std.map._
import scalaz.syntax.std.string._
import scalaz.syntax.equal._
import scalaz.syntax.show._
import scalaz.syntax.applicative._
import scalaz.syntax.traverse._
import java.time.Instant

import dhs.ImageFileId

@SuppressWarnings(Array("org.wartremover.warts.PublicInference", "org.wartremover.warts.IsInstanceOf"))
object Model {
  // We use this to avoid a dependency on spModel, should be replaced by gem
  sealed trait SeqexecSite {
    def instruments: NonEmptyList[Instrument]
  }
  object SeqexecSite {
    case object SeqexecGN extends SeqexecSite {
      val instruments: NonEmptyList[Instrument] = Instrument.gnInstruments
    }

    case object SeqexecGS extends SeqexecSite {
      val instruments : NonEmptyList[Instrument]= Instrument.gsInstruments
    }

    implicit val show: Show[SeqexecSite] = Show.shows({
      case SeqexecGN => "GN"
      case SeqexecGS => "GS"
    })
  }

  sealed trait ServerLogLevel
  object ServerLogLevel {
    case object INFO extends ServerLogLevel
    case object WARN extends ServerLogLevel
    case object ERROR extends ServerLogLevel
  }

  sealed trait SeqexecEvent
  sealed trait SeqexecModelUpdate extends SeqexecEvent {
    def view: SequencesQueue[SequenceView]
  }

  object SeqexecModelUpdate {
    implicit val equal: Equal[SeqexecModelUpdate] = Equal.equalA
  }
  object SeqexecEvent {
    final case class ConnectionOpenEvent(u: Option[UserDetails]) extends SeqexecEvent

    final case class SequenceStart(view: SequencesQueue[SequenceView]) extends SeqexecModelUpdate

    object SequenceStart {
      implicit val equal: Equal[SequenceStart] = Equal.equalA
    }

    final case class StepExecuted(view: SequencesQueue[SequenceView]) extends SeqexecModelUpdate

    final case class FileIdStepExecuted(fileId: ImageFileId, view: SequencesQueue[SequenceView]) extends SeqexecModelUpdate

    final case class SequenceCompleted(view: SequencesQueue[SequenceView]) extends SeqexecModelUpdate

    final case class SequenceLoaded(obsId: SequenceId, view: SequencesQueue[SequenceView]) extends SeqexecModelUpdate

    final case class SequenceUnloaded(obsId: SequenceId, view: SequencesQueue[SequenceView]) extends SeqexecModelUpdate

    final case class StepBreakpointChanged(view: SequencesQueue[SequenceView]) extends SeqexecModelUpdate

    final case class OperatorUpdated(view: SequencesQueue[SequenceView]) extends SeqexecModelUpdate

    final case class ObserverUpdated(view: SequencesQueue[SequenceView]) extends SeqexecModelUpdate

    final case class ConditionsUpdated(view: SequencesQueue[SequenceView]) extends SeqexecModelUpdate

    final case class StepSkipMarkChanged(view: SequencesQueue[SequenceView]) extends SeqexecModelUpdate

    final case class SequencePauseRequested(view: SequencesQueue[SequenceView]) extends SeqexecModelUpdate

    final case class SequencePauseCanceled(view: SequencesQueue[SequenceView]) extends SeqexecModelUpdate

    final case class SequenceRefreshed(view: SequencesQueue[SequenceView]) extends SeqexecModelUpdate

    final case class ActionStopRequested(view: SequencesQueue[SequenceView]) extends SeqexecModelUpdate

    final case class ResourcesBusy(obsId: SequenceId, view: SequencesQueue[SequenceView]) extends SeqexecModelUpdate

    final case class SequenceUpdated(view: SequencesQueue[SequenceView]) extends SeqexecModelUpdate

    // TODO: msg should be LogMsg but it does IO when getting a timestamp, it
    // has to be embedded in a `Task`
    final case class NewLogMessage(msg: String) extends SeqexecEvent
    final case class ServerLogMessage(level: ServerLogLevel, timestamp: Instant, msg: String) extends SeqexecEvent
    case object NullEvent extends SeqexecEvent

    implicit val equal: Equal[SeqexecEvent] = Equal.equalA
  }

  // The system name in ocs is a string but we can represent the important ones as an ADT
  sealed trait SystemName {
    def withParam(p: String): String = s"${this.shows}:$p"
  }
  object SystemName {
    case object ocs extends SystemName
    case object observe extends SystemName
    case object instrument extends SystemName
    case object telescope extends SystemName
    case object gcal extends SystemName
    case object calibration extends SystemName
    case object meta extends SystemName

    def unsafeFromString(system: String): SystemName = system match {
      case "ocs"         => ocs
      case "instrument"  => instrument
      case "telescope"   => telescope
      case "gcal"        => gcal
      case "observe"     => observe
      case "calibration" => calibration
      case "meta"        => meta
      case s             => sys.error(s"Unknown system name $s")
    }

    val all: List[SystemName] = List(ocs, instrument, telescope, gcal)

    implicit val show: Show[SystemName] = Show.shows {
      case `ocs`         => "ocs"
      case `instrument`  => "instrument"
      case `telescope`   => "telescope"
      case `gcal`        => "gcal"
      case `observe`     => "observe"
      case `calibration` => "calibration"
      case `meta`        => "meta"
    }
    implicit val equal: Equal[SystemName] = Equal.equalA[SystemName]
  }
  type ParamName = String
  type ParamValue = String
  type Parameters = Map[ParamName, ParamValue]
  type StepConfig = Map[SystemName, Parameters]
  implicit val stEqual: Equal[StepConfig] = Equal.equalA[StepConfig]
  // TODO This should be a richer type
  type SequenceId = String
  type StepId = Int
  type ObservationName = String
  type TargetName = String
  /**
    * A Seqexec resource represents any system that can be only used by one single agent.
    *
    */
  sealed trait Resource

  object Resource {

    case object P1 extends Resource
    case object OI extends Resource
    // Mount and science fold cannot be controlled independently. Maybe in the future.
    // For now, I replaced them with TCS
  //  case object Mount extends Resource
  //  case object ScienceFold extends Resource
    case object TCS extends Resource
    case object Gcal extends Resource
    case object Gems extends Resource
    case object Altair extends Resource

    implicit val order: Order[Resource] = Order.orderBy {
      case TCS               => 1
      case Gcal              => 2
      case Gems              => 3
      case Altair            => 4
      case OI                => 5
      case P1                => 6
      case Instrument.F2     => 11
      case Instrument.GmosS  => 12
      case Instrument.GmosN  => 13
      case Instrument.GPI    => 14
      case Instrument.GSAOI  => 15
      case Instrument.GNIRS  => 16
      case Instrument.NIRI   => 17
      case Instrument.NIFS   => 18
    }
    implicit val ordering: scala.math.Ordering[Resource] = order.toScalaOrdering
  }
  sealed trait Instrument extends Resource
  object Instrument {
    case object F2 extends Instrument
    case object GmosS extends Instrument
    case object GmosN extends Instrument
    case object GNIRS extends Instrument
    case object GPI extends Instrument
    case object GSAOI extends Instrument
    case object NIRI extends Instrument
    case object NIFS extends Instrument

    implicit val equal: Equal[Instrument] = Equal.equalA[Instrument]
    implicit val show: Show[Instrument] = Show.shows({
      case F2    => "Flamingos2"
      case GmosS => "GMOS-S"
      case GmosN => "GMOS-N"
      case GPI   => "GPI"
      case GSAOI => "GSAOI"
      case GNIRS => "GNIRS"
      case NIRI  => "NIRI"
      case NIFS  => "NIFS"
    })
    val gsInstruments: NonEmptyList[Instrument] = NonEmptyList[Instrument](F2, GmosS, GPI, GSAOI)
    val gnInstruments: NonEmptyList[Instrument] = NonEmptyList[Instrument](GmosN, GNIRS, NIRI, NIFS)
  }

  final case class Operator(value: String)

  object Operator {
    val Zero: Operator = Operator("")
    implicit val equal: Equal[Operator] = Equal.equalA
    implicit val shows: Show[Operator] = Show.shows(_.value)
  }

  final case class Observer(value: String)
  object Observer {
    val Zero: Observer = Observer("")
    implicit val equal: Equal[Observer] = Equal.equalA
    implicit val shows: Show[Observer] = Show.shows(_.value)
  }

  implicit val equalSequenceId: Equal[SequenceId] = Equal.equalA[SequenceId]

  sealed trait StepState {
    def canRunFrom: Boolean = false
  }
  object StepState {
    case object Pending extends StepState {
      override val canRunFrom: Boolean = true
    }
    case object Completed extends StepState
    case object Skipped extends StepState
    final case class Error(msg: String) extends StepState {
      override val canRunFrom: Boolean = true
    }
    case object Running extends StepState
    case object Paused extends StepState {
      override val canRunFrom: Boolean = true
    }

    implicit val equal: Equal[StepState] = Equal.equalA[StepState]
  }

  sealed trait ActionStatus
  object ActionStatus {
    case object Pending extends ActionStatus
    case object Completed extends ActionStatus
    case object Running extends ActionStatus

    implicit val equal: Equal[ActionStatus] = Equal.equalA[ActionStatus]
  }

  sealed trait Step {
    val id: StepId
    val config: StepConfig
    val status: StepState
    val breakpoint: Boolean
    val skip: Boolean
    val fileId: Option[dhs.ImageFileId]
  }
  object Step {
    implicit val equal: Equal[Step] = Equal.equalA[Step]
  }

  final case class StandardStep(
    override val id: StepId,
    override val config: StepConfig,
    override val status: StepState,
    override val breakpoint: Boolean,
    override val skip: Boolean,
    override val fileId: Option[dhs.ImageFileId],
    configStatus: List[(Resource, ActionStatus)],
    observeStatus: ActionStatus
  ) extends Step
  object StandardStep {
    implicit val equal: Equal[StandardStep] = Equal.equalA[StandardStep]
  }
  // Other kinds of Steps to be defined.

  sealed trait SequenceState
  object SequenceState {
    case object Completed         extends SequenceState
    case object Running           extends SequenceState
    case object Pausing           extends SequenceState
    case object Stopping          extends SequenceState
    case object Idle              extends SequenceState
    case object Paused            extends SequenceState
    final case class Error(msg: String) extends SequenceState

    def isError(state: SequenceState): Boolean = state match {
      case Error(_) => true
      case _        => false
    }

    implicit val equal: Equal[SequenceState] = Equal.equalA[SequenceState]
  }

  /**
    * Metadata about the sequence required on the exit point
    */
  // TODO Une a proper instrument class
  @Lenses final case class SequenceMetadata(
    instrument: Instrument,
    observer: Option[Observer],
    name: String
  )

  @Lenses final case class SequenceView (
    id: SequenceId,
    metadata: SequenceMetadata,
    status: SequenceState,
    steps: List[Step],
    willStopIn: Option[Int]
  )

  object SequenceView {
    implicit val eq: Equal[SequenceView] = Equal.equalA
  }

  /**
    * Represents a queue with different levels of details. E.g. it could be a list of Ids
    * Or a list of fully hydrated SequenceViews
    */
  final case class SequencesQueue[T](conditions: Conditions, operator: Option[Operator], queue: List[T])

  object SequencesQueue {
    implicit def equal[T: Equal]: Equal[SequencesQueue[T]] = Equal.equalA
  }

  // Complements to the science model
  sealed trait StepType
  object StepType {
    case object Object extends StepType
    case object Arc extends StepType
    case object Flat extends StepType
    case object Bias extends StepType
    case object Dark extends StepType
    case object Calibration extends StepType

    implicit val eq: Equal[StepType] = Equal.equalA[StepType]
    implicit val show: Show[StepType] = Show.shows {
      case Object      => "OBJECT"
      case Arc         => "ARC"
      case Flat        => "FLAT"
      case Bias        => "BIAS"
      case Dark        => "DARK"
      case Calibration => "CAL"
    }

    val all: List[StepType] = List(Object, Arc, Flat, Bias, Dark, Calibration)
    private val names = all.map(x => (x.shows, x)).toMap

    def fromString(s: String): Option[StepType] = names.get(s)
  }
  sealed trait OffsetAxis {
    val configItem: String
  }
  object OffsetAxis {
    case object AxisP extends OffsetAxis {
      val configItem = "p"
    }
    case object AxisQ extends OffsetAxis {
      val configItem = "q"
    }
    implicit val show: Show[OffsetAxis] = Show.shows {
      case AxisP => "p"
      case AxisQ => "q"
    }
  }

  sealed trait Offset {
    val value: Double
  }
  object Offset {
    implicit val equal: Equal[Offset] = Equal.equalA
    def Zero(axis: OffsetAxis): Offset = axis match {
      case OffsetAxis.AxisP => TelescopeOffset.P.Zero
      case OffsetAxis.AxisQ => TelescopeOffset.Q.Zero
    }
  }

  // Telescope offsets, roughly based on gem
  final case class TelescopeOffset(p: TelescopeOffset.P, q: TelescopeOffset.Q)
  object TelescopeOffset {
    /** P component of an angular offset.. */
    final case class P(value: Double) extends Offset
    object P {
      val Zero: P = P(0.0)
      implicit val order: Order[P] = Order.orderBy(_.value)

    }
    /** Q component of an angular offset.. */
    final case class Q(value: Double) extends Offset
    object Q {
      val Zero: Q = Q(0.0)
      implicit val order: Order[Q] = Order.orderBy(_.value)

    }
    implicit val eq: Equal[TelescopeOffset] = Equal.equalA[TelescopeOffset]
    implicit val show: Show[TelescopeOffset] = Show.showFromToString
  }

  // Ported from OCS' SPSiteQuality.java

  final case class Conditions(
    cc: CloudCover,
    iq: ImageQuality,
    sb: SkyBackground,
    wv: WaterVapor
  )

  object Conditions {

    val worst: Conditions = Conditions(
      CloudCover.Any,
      ImageQuality.Any,
      SkyBackground.Any,
      WaterVapor.Any
    )

    val nominal: Conditions = Conditions(
      CloudCover.Percent50,
      ImageQuality.Percent70,
      SkyBackground.Percent50,
      WaterVapor.Any
    )

    val best: Conditions = Conditions(
      // In the ODB model it's 20% but that value it's marked as obsolete
      // so I took the non-obsolete lowest value.
      CloudCover.Percent50,
      ImageQuality.Percent20,
      SkyBackground.Percent20,
      WaterVapor.Percent20
    )

    val default: Conditions = worst // Taken from ODB

    implicit val equalConditions: Equal[Conditions] = Equal.equalA[Conditions]

    implicit val showConditions: Show[Conditions] = Show.shows[Conditions] {
      case Conditions(cc, iq, sb, wv) => List(cc, iq, sb, wv).mkString(", ")
    }

  }

  sealed trait CloudCover {
    val toInt: Int
  }
  object CloudCover {
    case object Percent50 extends CloudCover { override val toInt: Int = 50  }
    case object Percent70 extends CloudCover { override val toInt: Int = 70  }
    case object Percent80 extends CloudCover { override val toInt: Int = 80  }
    case object Any       extends CloudCover { override val toInt: Int = 100 } // ODB is 100

    val all: List[CloudCover] = List(Percent50, Percent70, Percent80, Any)

    implicit val equalCloudCover: Equal[CloudCover] = Equal.equalA[CloudCover]

    implicit val showCloudCover: Show[CloudCover] = Show.shows[CloudCover] {
      case Percent50 => "50%/Clear"
      case Percent70 => "70%/Cirrus"
      case Percent80 => "80%/Cloudy"
      case Any       => "Any"
    }

  }

  sealed trait ImageQuality {
    val toInt: Int
  }
  object ImageQuality {
    case object Percent20 extends ImageQuality { override val toInt: Int = 20  }
    case object Percent70 extends ImageQuality { override val toInt: Int = 70  }
    case object Percent85 extends ImageQuality { override val toInt: Int = 85  }
    case object Any       extends ImageQuality { override val toInt: Int = 100 } // ODB is 100

    val all: List[ImageQuality] = List(Percent20, Percent70, Percent85, Any)

    implicit val equalImageQuality: Equal[ImageQuality] = Equal.equalA[ImageQuality]

    implicit val showImageQuality: Show[ImageQuality] = Show.shows[ImageQuality] {
      case Percent20 => "20%/Best"
      case Percent70 => "70%/Good"
      case Percent85 => "85%/Poor"
      case Any       => "Any"
    }

  }

  sealed trait SkyBackground {
    val toInt: Int
  }
  object SkyBackground {
    case object Percent20 extends SkyBackground { override val toInt: Int = 20  }
    case object Percent50 extends SkyBackground { override val toInt: Int = 50  }
    case object Percent80 extends SkyBackground { override val toInt: Int = 80  }
    case object Any       extends SkyBackground { override val toInt: Int = 100 } // ODB is 100

    val all: List[SkyBackground] = List(Percent20, Percent50, Percent80, Any)

    implicit val equal: Equal[SkyBackground] = Equal.equalA[SkyBackground]

    implicit val showSkyBackground: Show[SkyBackground] = Show.shows[SkyBackground] {
      case Percent20 => "20%/Darkest"
      case Percent50 => "50%/Dark"
      case Percent80 => "80%/Grey"
      case Any       => "Any/Bright"
    }

  }

  sealed trait WaterVapor {
    val toInt: Int
  }
  object WaterVapor {
    case object Percent20 extends WaterVapor { override val toInt: Int = 20  }
    case object Percent50 extends WaterVapor { override val toInt: Int = 50  }
    case object Percent80 extends WaterVapor { override val toInt: Int = 80  }
    case object Any       extends WaterVapor { override val toInt: Int = 100 } // ODB is 100

    val all: List[WaterVapor] = List(Percent20, Percent50, Percent80, Any)

    implicit val equal: Equal[WaterVapor] = Equal.equalA[WaterVapor]

    implicit val showWaterVapor: Show[WaterVapor] = Show.shows[WaterVapor] {
      case Percent20 => "20%/Low"
      case Percent50 => "50%/Median"
      case Percent80 => "85%/High"
      case Any       => "Any"
    }

  }

  // Log message types
  type Time = java.time.Instant

  trait LogType
  object LogType {
    object Debug
    object Info
    object Warning
    object Error
  }

  final case class LogMsg(t: LogType, timestamp: Time, msg: String)

}

trait ModelLenses {
  import Model._
  import Model.SeqexecEvent._

    // Some useful Monocle lenses
  val obsNameL: Lens[SequenceView, String] = GenLens[SequenceView](_.metadata.name)
  // From step to standard step
  val standardStepP: Prism[Step, StandardStep] = GenPrism[Step, StandardStep]
  val eachStepT: Traversal[List[Step], Step] = Traversal.fromTraverse[List, Step]
  val obsStepsL: Lens[SequenceView, List[Step]] = GenLens[SequenceView](_.steps)
  val eachViewT: Traversal[List[SequenceView], SequenceView] = Traversal.fromTraverse[List, SequenceView]
  val sequencesQueueL: Lens[SequencesQueue[SequenceView], List[SequenceView]] = GenLens[SequencesQueue[SequenceView]](_.queue)
  // from standard step to config
  val stepConfigL: Lens[StandardStep, StepConfig] = GenLens[StandardStep](_.config)
  // Prism to focus on only the SeqexecEvents that have a queue
  val sequenceEventsP: Prism[SeqexecEvent, SeqexecModelUpdate] = GenPrism[SeqexecEvent, SeqexecModelUpdate]
  // Required for type correctness
  val stepConfigRoot: Iso[Map[SystemName, Parameters], Map[SystemName, Parameters]] = Iso.id[Map[SystemName, Parameters]]
  val parametersRoot: Iso[Map[ParamName, ParamValue], Map[ParamName, ParamValue]] = Iso.id[Map[ParamName, ParamValue]]

  // Focus on a param value
  def paramValueL(param: ParamName): Lens[Parameters, Option[String]] =
    parametersRoot ^|-> // map of parameters
    at(param)           // parameter containing the name

  // Possible set of observe parameters
  def systemConfigL(system: SystemName): Lens[StepConfig, Option[Parameters]] =
    stepConfigRoot ^|-> // map of systems
    at(system)          // subsystem name

  // Param name of a StepConfig
  def configParamValueO(system: SystemName, param: String): Optional[StepConfig, String] =
    systemConfigL(system)                ^<-? // observe paramaters
    some                                 ^|-> // focus on the option
    paramValueL(system.withParam(param)) ^<-? // find the target name
    some                                      // focus on the option

  // Focus on the sequence view
  val sequenceQueueViewL: Lens[SeqexecModelUpdate, SequencesQueue[SequenceView]] = Lens[SeqexecModelUpdate, SequencesQueue[SequenceView]](_.view)(q => {
      case e @ SequenceStart(_)           => e.copy(view = q)
      case e @ StepExecuted(_)            => e.copy(view = q)
      case e @ FileIdStepExecuted(_, _)   => e.copy(view = q)
      case e @ SequenceCompleted(_)       => e.copy(view = q)
      case e @ SequenceLoaded(_, v)       => e.copy(view = q)
      case e @ SequenceUnloaded(_, v)     => e.copy(view = q)
      case e @ StepBreakpointChanged(_)   => e.copy(view = q)
      case e @ OperatorUpdated(_)         => e.copy(view = q)
      case e @ ObserverUpdated(_)         => e.copy(view = q)
      case e @ ConditionsUpdated(_)       => e.copy(view = q)
      case e @ StepSkipMarkChanged(_)     => e.copy(view = q)
      case e @ SequencePauseRequested(_)  => e.copy(view = q)
      case e @ SequencePauseCanceled(_)   => e.copy(view = q)
      case e @ SequenceRefreshed(_)       => e.copy(view = q)
      case e @ ActionStopRequested(_)     => e.copy(view = q)
      case e @ ResourcesBusy(_, _)        => e.copy(view = q)
      case e @ SequenceUpdated(_)         => e.copy(view = q)
      case e                              => e
    }
  )
  // Composite lens to change the sequence name of an event
  val sequenceNameT: Traversal[SeqexecEvent, ObservationName] =
    sequenceEventsP         ^|->  // Events with model updates
    sequenceQueueViewL         ^|->  // Find the sequence view
    sequencesQueueL ^|->> // Find the queue
    eachViewT       ^|->  // each sequence on the queue
    obsNameL              // sequence's observation name

  // Composite lens to find the step config
  val sequenceConfigT: Traversal[SeqexecEvent, StepConfig] =
    sequenceEventsP           ^|->  // Events with model updates
    sequenceQueueViewL           ^|->  // Find the sequence view
    sequencesQueueL   ^|->> // Find the queue
    eachViewT         ^|->  // each sequence on the queue
    obsStepsL         ^|->> // sequence steps
    eachStepT         ^<-?  // each step
    standardStepP     ^|->  // which is a standard step
    stepConfigL             // configuration of the step

  def filterEntry[K, V](predicate: (K, V) => Boolean): Traversal[Map[K, V], V] =
    new Traversal[Map[K, V], V]{
      def modifyF[F[_]: Applicative](f: V => F[V])(s: Map[K, V]): F[Map[K, V]] =
        s.map { case (k, v) =>
          k -> (if(predicate(k, v)) f(v) else v.pure[F])
        }.sequenceU
    }

  // Find the Parameters of the steps containing science steps
  val scienceStepT: Traversal[StepConfig, Parameters] = filterEntry[SystemName, Parameters] {
    case (s, p) => s === SystemName.observe && p.exists {
      case (k, v) => k === SystemName.observe.withParam("observeType") && v === "OBJECT"
    }
  }

  val scienceTargetNameO: Optional[Parameters, TargetName] =
    paramValueL(SystemName.observe.withParam("object")) ^<-? // find the target name
    some                                                     // focus on the option

  val stringToStepTypeP: Prism[String, StepType] = Prism(StepType.fromString)(_.shows)
  private[model] def telescopeOffsetPI: Iso[Double, TelescopeOffset.P] = Iso(TelescopeOffset.P.apply)(_.value)
  private[model] def telescopeOffsetQI: Iso[Double, TelescopeOffset.Q] = Iso(TelescopeOffset.Q.apply)(_.value)
  val stringToDoubleP: Prism[String, Double] = Prism((x: String) => x.parseDouble.toOption)(_.shows)

  val stepTypeO: Optional[Step, StepType] =
    standardStepP                                            ^|-> // which is a standard step
    stepConfigL                                              ^|-> // configuration of the step
    systemConfigL(SystemName.observe)                        ^<-? // Observe config
    some                                                     ^|-> // some
    paramValueL(SystemName.observe.withParam("observeType")) ^<-? // find the target name
    some                                                     ^<-? // focus on the option
    stringToStepTypeP                                             // step type

  // Lens to find p offset
  def telescopeOffsetO(x: OffsetAxis): Optional[Step, Double] =
    standardStepP                                             ^|-> // which is a standard step
    stepConfigL                                               ^|-> // configuration of the step
    systemConfigL(SystemName.telescope)                       ^<-? // Observe config
    some                                                      ^|-> // some
    paramValueL(SystemName.telescope.withParam(x.configItem)) ^<-? // find the offset
    some                                                      ^<-? // focus on the option
    stringToDoubleP                                                // double value

  val telescopeOffsetPO: Optional[Step, TelescopeOffset.P] = telescopeOffsetO(OffsetAxis.AxisP) ^<-> telescopeOffsetPI
  val telescopeOffsetQO: Optional[Step, TelescopeOffset.Q] = telescopeOffsetO(OffsetAxis.AxisQ) ^<-> telescopeOffsetQI

  // Composite lens to find the step config
  val firstScienceTargetNameT: Traversal[SeqexecEvent, TargetName] =
    sequenceConfigT     ^|->> // sequence configuration
    scienceStepT        ^|-?  // science steps
    scienceTargetNameO        // science target name

  // Composite lens to find the target name on observation
  val observeTargetNameT: Traversal[SeqexecEvent, TargetName] =
    sequenceConfigT                                 ^|-?  // configuration of the step
    configParamValueO(SystemName.observe, "object")       // on the configuration find the target name

  // Composite lens to find the target name on telescope
  val telescopeTargetNameT: Traversal[SeqexecEvent, TargetName] =
    sequenceConfigT                                       ^|-?  // configuration of the step
    configParamValueO(SystemName.telescope, "Base:name")        // on the configuration find the target name


  // Composite lens to find the first science step and from there the target name
  val firstScienceStepTargetNameT: Traversal[SequenceView, TargetName] =
    obsStepsL           ^|->> // observation steps
    eachStepT           ^<-?  // each step
    standardStepP       ^|->  // only standard steps
    stepConfigL         ^|->> // get step config
    scienceStepT        ^|-?  // science steps
    scienceTargetNameO        // science target name

}
