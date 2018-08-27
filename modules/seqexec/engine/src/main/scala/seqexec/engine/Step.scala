// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.engine

import cats.implicits._
import gem.math.Index
import monocle.Lens
import monocle.macros.GenLens
import seqexec.model.StepState

/**
  * A list of `Executions` grouped by observation.
  */
final case class Step(
  id: Index,
  fileId: Option[FileId],
  breakpoint: Step.BreakpointMark,
  skipped: Step.Skipped,
  skipMark: Step.SkipMark,
  executions: List[List[Action]]
)

object Step {

  final case class BreakpointMark(self: Boolean) extends AnyVal
  final case class SkipMark(self: Boolean) extends AnyVal
  final case class Skipped(self: Boolean) extends AnyVal

  def init(id: Index,
           fileId: Option[FileId],
           executions: List[List[Action]]): Step = Step(id, fileId, BreakpointMark(false), Skipped(false), SkipMark(false), executions)

  /**
    * Calculate the `Step` `Status` based on the underlying `Action`s.
    */
  def status(step: Step): StepState = {

    if(step.skipped.self) StepState.Skipped
    else
      // Find an error in the Step
      step.executions.flatten.find(Action.errored).flatMap { x => x.state.runState match {
        case Action.Failed(Result.Error(msg)) => msg.some
        case _                                => None
        // Return error or continue with the rest of the checks
      }}.map(StepState.Failed).getOrElse(
        // All actions in this Step were completed successfully, or the Step is empty.
        if (step.executions.flatten.forall(Action.completed)) StepState.Completed
        else if (step.executions.flatten.forall(_.state.runState.isIdle)) StepState.Pending
        // Not all actions are completed or pending.
        else StepState.Running
      )

  }

  /**
    * Step Zipper. This structure is optimized for the actual `Step` execution.
    *
    */
  final case class Zipper(
    id: Index,
    fileId: Option[FileId],
    breakpoint: BreakpointMark,
    skipMark: SkipMark,
    pending: List[Actions],
    focus: Execution,
    done: List[Actions],
    rolledback: (Execution, List[Actions])
  ) { self =>

    /**
      * Adds the `Current` `Execution` to the list of completed `Execution`s and
      * makes the next pending `Execution` the `Current` one.
      *
      * If there are still `Action`s that have not finished in `Current` or if
      * there are no more pending `Execution`s it returns `None`.
      */
    val next: Option[Zipper] =
      pending match {
        case Nil           => None
        case exep :: exeps =>
          (Execution.currentify(exep), focus.uncurrentify).mapN (
            (curr, exed) => self.copy(pending = exeps, focus = curr, done = exed :: done)
          )
      }

    def rollback: Zipper =
      self.copy(pending = rolledback._2, focus = rolledback._1, done = Nil)

    /**
      * Obtain the resulting `Step` only if all `Execution`s have been completed.
      * This is a special way of *unzipping* a `Zipper`.
      *
      */
    val uncurrentify: Option[Step] =
      if (pending.isEmpty) focus.uncurrentify.map(
        x => Step(id, fileId, breakpoint, Skipped(false), skipMark, x :: done)
      )
      else None

    /**
      * Unzip a `Zipper`. This creates a single `Step` with either completed
      * `Exection`s or pending `Execution`s.
      */
    val toStep: Step =
      Step(
        id,
        fileId,
        breakpoint,
        Skipped(false),
        skipMark,
        done ++ List(focus.execution) ++ pending
      )

    val skip: Step = toStep.copy(skipped = Skipped(true))

    def update(step: Step): Zipper = {
      val currentified = Zipper.currentify(step)

      //If running, only change the pending executions and the rollback definition.
      (if (Step.status(toStep) === StepState.Running)
        // Step updates should not change the number of Executions. If it does, the update will not apply unless
        // the Step is paused and restarted.
        if (step.executions.length === done.length + pending.length + 1)
          currentified.map(c => this.copy(pending = c.pending.takeRight(pending.length), rolledback = c.rolledback))
        else
          currentified.map(c => this.copy(rolledback = c.rolledback))
      else currentified.map(_.copy(breakpoint = this.breakpoint, skipMark = this.skipMark))
      ).getOrElse(this)
    }

  }

  object Zipper {

    /**
      * Make a `Zipper` from a `Step` only if all the `Execution`s in the `Step` are
      * pending. This is a special way of *zipping* a `Step`.
      *
      */
    def currentify(step: Step): Option[Zipper] =
      step.executions match {
        case Nil         => None
        case exe :: exes =>
          Execution.currentify(exe).map(x =>
            Zipper(
              step.id,
              step.fileId,
              step.breakpoint,
              step.skipMark,
              exes,
              x,
              Nil,
              (x, exes)
            )
          )
      }

    val current: Lens[Zipper, Execution] =
      GenLens[Zipper](_.focus)

    val fileId: Lens[Zipper, Option[FileId]] =
      GenLens[Zipper](_.fileId)

  }

}
