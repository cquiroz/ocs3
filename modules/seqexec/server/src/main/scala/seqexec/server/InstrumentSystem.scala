// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server

import edu.gemini.spModel.config2.Config
import fs2.Stream
import gem.enum.LightSinkName
import seqexec.engine.Action
import seqexec.engine.Result
import seqexec.model.dhs.ImageFileId
import seqexec.model.enum.Instrument
import seqexec.model.enum.ObserveCommandResult
import seqexec.server.keywords.KeywordsClient
import squants.{Length, Time}

trait InstrumentActions[F[_]] {
  def observeActions(stepType: StepType, result: Stream[F, Result[F]]): List[List[Action[F]]]
  def initialStepAction(stepType: StepType, action: Action[F]): List[List[Action[F]]]
}

trait InstrumentSystem[F[_]] extends System[F] with InstrumentGuide with InstrumentActions[F] {
  override val resource: Instrument
  // The name used for this instrument in the science fold configuration
  def sfName(config: Config): LightSinkName
  val contributorName: String
  val observeControl: InstrumentSystem.ObserveControl[F]

  def observe(
      config: Config): SeqObserveF[F, ImageFileId, ObserveCommandResult]
  //Expected total observe lapse, used to calculate timeout
  def calcObserveTime(config: Config): F[Time]
  def keywordsClient: KeywordsClient[F]
  def observeProgress(total: Time, elapsed: InstrumentSystem.ElapsedTime): Stream[F, Progress]
  def calcStepType(config: Config): Either[SeqexecFailure, StepType] =
    SequenceConfiguration.calcStepType(config)
  def initialStepAction(stepType: StepType, action: Action[F]): List[List[Action[F]]] =
    List(List(action))
  override def observeActions(stepType: StepType, result: Stream[F, Result[F]]): List[List[Action[F]]] =
    SequenceActions.observeActions(result)
  override val oiOffsetGuideThreshold: Option[Length] = None
  override def instrument: Instrument = resource
}

object InstrumentSystem {

  final case class StopObserveCmd[F[_]](self: SeqActionF[F, Unit])
  final case class AbortObserveCmd[F[_]](self: SeqActionF[F, Unit])
  final case class PauseObserveCmd[F[_]](self: SeqActionF[F, Unit])
  final case class ContinuePausedCmd[F[_]](
      self: Time => SeqActionF[F, ObserveCommandResult])
  final case class StopPausedCmd[F[_]](self: SeqActionF[F, ObserveCommandResult])
  final case class AbortPausedCmd[F[_]](self: SeqActionF[F, ObserveCommandResult])

  sealed trait ObserveControl[+F[_]] extends Product with Serializable
  case object Uncontrollable extends ObserveControl[Nothing]
  final case class CompleteControl[F[_]](stop: StopObserveCmd[F],
                                abort: AbortObserveCmd[F],
                                pause: PauseObserveCmd[F],
                                continue: ContinuePausedCmd[F],
                                stopPaused: StopPausedCmd[F],
                                abortPaused: AbortPausedCmd[F])
      extends ObserveControl[F]
  // Special class for instrument, that cannot pause/resume like IR instruments and GSAOI
  final case class UnpausableControl[F[_]](stop: StopObserveCmd[F], abort: AbortObserveCmd[F])
      extends ObserveControl[F]

  final case class ElapsedTime(self: Time) extends AnyVal
}
