// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server

import cats.Show
import cats.effect.Timer
import cats.effect.Sync
import cats.implicits._
import fs2.Stream
import gov.aps.jca.TimeoutException
import java.util.concurrent.atomic.AtomicBoolean
import java.util.concurrent.atomic.AtomicInteger
import mouse.all._
import org.log4s.getLogger
import seqexec.model.dhs.ImageFileId
import seqexec.model.enum.ObserveCommandResult
import seqexec.server.SeqexecFailure.SeqexecException
import seqexec.server.InstrumentSystem.ElapsedTime
import squants.time.{Seconds, Time}
import scala.concurrent.duration._
import scala.annotation.tailrec

sealed trait InstrumentControllerSim[F[_]] {
  def observe(fileId: ImageFileId, expTime: Time): F[ObserveCommandResult]

  def applyConfig[C: Show](config: C): F[Unit]

  def stopObserve: F[Unit]

  def abortObserve: F[Unit]

  def endObserve: F[Unit]

  def pauseObserve: F[Unit]

  def resumePaused: F[ObserveCommandResult]

  def stopPaused: F[ObserveCommandResult]

  def abortPaused: F[ObserveCommandResult]

  def observeCountdown(total: Time, elapsed: ElapsedTime): Stream[F, Progress]

}

object InstrumentControllerSim {
  private final class InstrumentControllerSimImpl[F[_]](name: String, useTimeout: Boolean)(implicit val F: Sync[F], T: Timer[F]) extends InstrumentControllerSim[F] {
    private val Log = getLogger

    private val stopFlag = new AtomicBoolean(false)
    private val abortFlag = new AtomicBoolean(false)
    private val pauseFlag = new AtomicBoolean(false)
    private val remainingTime = new AtomicInteger(0)

    private val tic = 200

    private val ReadoutDelay = Seconds(5)
    private val ConfigurationDelay = FiniteDuration(5, SECONDS)
    private val StopObserveDelay = FiniteDuration(1500, MILLISECONDS)

    def log(msg: => String): F[Unit] =
      F.delay(Log.info(msg))

    @tailrec
    private def observeTic(stop: Boolean, abort: Boolean, pause: Boolean, remain: Int, timeout: Option[Int]): F[ObserveCommandResult] =
      if(remain < tic) {
        log(s"Simulate $name observation completed") *>
        ObserveCommandResult.Success.pure[F].widen
      } else if(stop) ObserveCommandResult.Stopped.pure[F].widen
        else if(abort) ObserveCommandResult.Aborted.pure[F].widen
        else if(pause) {
          F.delay(remainingTime.set(remain)) *>
          ObserveCommandResult.Paused.pure[F].widen
        }
        else if(timeout.exists(_<= 0)) F.raiseError(SeqexecException(new TimeoutException()))
        else {
          Thread.sleep(tic.toLong)
          observeTic(stopFlag.get, abortFlag.get, pauseFlag.get, remain - tic, timeout.map(_ - tic))
        }

    def observe(fileId: ImageFileId, expTime: Time): F[ObserveCommandResult] =
      log(s"Simulate taking $name observation with label $fileId") *> F.delay {
        pauseFlag.set(false)
        stopFlag.set(false)
        abortFlag.set(false)
        val totalTime = (expTime + ReadoutDelay).toMilliseconds.toInt
        remainingTime.set(totalTime)
        totalTime
      }.flatMap {totalTime =>
        observeTic(stop = false, abort = false, pause = false, totalTime, useTimeout.option(totalTime + 2 * tic))
      }

    def applyConfig[C: Show](config: C): F[Unit] =
      log(s"Simulate applying $name configuration ${config.show}") *>
        T.sleep(ConfigurationDelay)

    def stopObserve: F[Unit] =
      log(s"Simulate stopping $name exposure") *>
        T.sleep(StopObserveDelay) *>
        F.delay(stopFlag.set(true))

    def abortObserve: F[Unit] =
      log(s"Simulate aborting $name exposure") *>
      F.delay(abortFlag.set(true))

    def endObserve: F[Unit] =
      log(s"Simulate sending endObserve to $name")

    def pauseObserve: F[Unit] =
      log(s"Simulate pausing $name exposure")
        F.delay(pauseFlag.set(true))

    def resumePaused: F[ObserveCommandResult] =
      log(s"Simulate resuming $name observation") *>
        F.delay(pauseFlag.set(false)) *>
        observeTic(stop = false, abort = false, pause = false, remainingTime.get,
        useTimeout.option(remainingTime.get + 2 * tic))

    def stopPaused: F[ObserveCommandResult] =
      log(s"Simulate stopping $name paused observation") *>
        F.delay(pauseFlag.set(false)) *>
        observeTic(stop = true, abort = false, pause = false, 1000, None)

    def abortPaused: F[ObserveCommandResult] =
      log(s"Simulate aborting $name paused observation") *>
        F.delay(pauseFlag.set(false)) *>
        observeTic(stop = false, abort = true, pause = false, 1000, None)

    def observeCountdown(total: Time, elapsed: ElapsedTime): Stream[F, Progress] =
      ProgressUtil.countdown[F](total, elapsed.self)

  }

  def apply[F[_]: Sync: Timer](name: String): InstrumentControllerSim[F] = new InstrumentControllerSimImpl[F](name, false)
  def withTimeout[F[_]: Sync: Timer](name: String): InstrumentControllerSim[F] = new InstrumentControllerSimImpl[F](name, true)

}
