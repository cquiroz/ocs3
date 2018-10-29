// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.web.client.circuit

import cats.Eq
import cats.implicits._
import cats.data.NonEmptyList
import gem.Observation
import monocle.Getter
import seqexec.model.enum._
import seqexec.web.client.model._

sealed trait TabContentFocus extends Product with Serializable {
  val canOperate: Boolean
  val logDisplayed: SectionVisibilityState
  val active: TabSelected
}

object TabContentFocus {
  implicit val eq: Eq[TabContentFocus] =
    Eq.instance {
      case (a: SequenceTabContentFocus, b: SequenceTabContentFocus) => a === b
      case (a: CalQueueTabContentFocus, b: CalQueueTabContentFocus) => a === b
      case _                                                        => false
    }

  val tabContentFocusG
    : Getter[SeqexecAppRootModel, NonEmptyList[TabContentFocus]] = {
    val getter = SeqexecAppRootModel.logDisplayL.asGetter
      .zip(SeqexecAppRootModel.sequencesOnDisplayL.asGetter)
    ClientStatus.canOperateG.zip(getter) >>> { p =>
      val (o, (log, SequencesOnDisplay(tabs))) = p
      NonEmptyList.fromListUnsafe(tabs.withFocus.toList.collect {
        case (tab: SequenceTab, active) =>
          SequenceTabContentFocus(o,
                                  tab.instrument,
                                  tab.sequence.map(_.id),
                                  tab.isComplete,
                                  TabSelected.fromBoolean(active),
                                  log)
        case (_: CalibrationQueueTab, active) =>
          CalQueueTabContentFocus(o, TabSelected.fromBoolean(active), log)
      })
    }
  }
}

final case class SequenceTabContentFocus(canOperate:   Boolean,
                                         instrument:   Option[Instrument],
                                         id:           Option[Observation.Id],
                                         completed:    Boolean,
                                         active:       TabSelected,
                                         logDisplayed: SectionVisibilityState)
    extends TabContentFocus

object SequenceTabContentFocus {
  implicit val eq: Eq[SequenceTabContentFocus] =
    Eq.by(x =>
      (x.canOperate, x.instrument, x.id, x.completed, x.active, x.logDisplayed))
}

final case class CalQueueTabContentFocus(canOperate:   Boolean,
                                         active:       TabSelected,
                                         logDisplayed: SectionVisibilityState)
    extends TabContentFocus

object CalQueueTabContentFocus {
  implicit val eq: Eq[CalQueueTabContentFocus] =
    Eq.by(x => (x.canOperate, x.active, x.logDisplayed))
}