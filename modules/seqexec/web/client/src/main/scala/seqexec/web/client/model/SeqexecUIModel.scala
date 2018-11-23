// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.web.client.model

import cats.Eq
import cats.implicits._
import monocle.macros.Lenses
import seqexec.model.Observer
import seqexec.model.UserDetails
import seqexec.web.common.FixedLengthBuffer
import seqexec.web.client.components.sequence.steps.StepConfigTable
import seqexec.web.client.components.SessionQueueTable
import web.client.table._

sealed trait SoundSelection extends Product with Serializable

object SoundSelection {
  case object SoundOn extends SoundSelection
  case object SoundOff extends SoundSelection

  implicit val eq: Eq[SoundSelection] = Eq.fromUniversalEquals

  def flip: SoundSelection => SoundSelection = _ match {
    case SoundSelection.SoundOn  => SoundSelection.SoundOff
    case SoundSelection.SoundOff => SoundSelection.SoundOn
  }
}

/**
  * UI model, changes here will update the UI
  */
@Lenses
final case class SeqexecUIModel(
  navLocation:        Pages.SeqexecPages,
  user:               Option[UserDetails],
  loginBox:           SectionVisibilityState,
  globalLog:          GlobalLog,
  sequencesOnDisplay: SequencesOnDisplay,
  configTableState:   TableState[StepConfigTable.TableColumn],
  queueTableState:    TableState[SessionQueueTable.TableColumn],
  defaultObserver:    Observer,
  notification:       UserNotificationState,
  queues:             CalibrationQueues,
  obsProgress:        AllObservationsProgressState,
  sessionQueueFilter: SessionQueueFilter,
  sound:              SoundSelection,
  firstLoad:          Boolean)

@SuppressWarnings(Array("org.wartremover.warts.PublicInference"))
object SeqexecUIModel {
  val Initial: SeqexecUIModel = SeqexecUIModel(
    Pages.Root,
    None,
    SectionClosed,
    GlobalLog(FixedLengthBuffer.unsafeFromInt(500), SectionClosed),
    SequencesOnDisplay.Empty,
    StepConfigTable.InitialTableState,
    SessionQueueTable.InitialTableState.tableState,
    Observer(""),
    UserNotificationState.Empty,
    CalibrationQueues.Default,
    AllObservationsProgressState.Empty,
    SessionQueueFilter.NoFilter,
    SoundSelection.SoundOn,
    firstLoad = true
  )

  implicit val eq: Eq[SeqexecUIModel] =
    Eq.by(
      x =>
        (x.navLocation,
         x.user,
         x.loginBox,
         x.globalLog,
         x.sequencesOnDisplay,
         x.configTableState,
         x.queueTableState,
         x.defaultObserver,
         x.notification,
         x.queues,
         x.sessionQueueFilter,
         x.sound,
         x.firstLoad))

  val defaultObserverG = SeqexecUIModel.defaultObserver.asGetter
}