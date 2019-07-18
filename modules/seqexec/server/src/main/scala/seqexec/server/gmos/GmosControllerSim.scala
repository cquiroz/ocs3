// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server.gmos

import cats.implicits._
import cats.effect.Sync
import cats.effect.Timer
import java.util.concurrent.atomic.AtomicBoolean
import org.log4s.getLogger
import seqexec.model.dhs.ImageFileId
import seqexec.model.enum.ObserveCommandResult
import seqexec.server.InstrumentSystem.ElapsedTime
import seqexec.server.gmos.GmosController.{GmosConfig, NorthTypes, SiteDependentTypes, SouthTypes}
import seqexec.server.{InstrumentControllerSim, Progress}
import squants.Time

object GmosControllerSim {
  def apply[F[_]: Sync: Timer, T <: SiteDependentTypes](name: String): GmosController[F, T] =
    new GmosController[F, T] {
      private val Log = getLogger
      private val isNS = new AtomicBoolean(false)

      def log(msg: => String): F[Unit] =
        Sync[F].delay(Log.info(msg))

      private val sim: InstrumentControllerSim[F] = InstrumentControllerSim[F](s"GMOS $name")

      override def observe(fileId: ImageFileId, expTime: Time): F[ObserveCommandResult] =
        if (isNS.get) {
          log(s"Simulate taking a Gmos N&S observation with label $fileId") *>
          sim.observe(fileId, expTime / 5).as(ObserveCommandResult.Paused)
        } else {
          sim.observe(fileId, expTime)
        }

      override def applyConfig(config: GmosConfig[T]): F[Unit] = {
        isNS.set(config.ns.nsPairs > 0)
        sim.applyConfig(config)
      }

      override def stopObserve: F[Unit] = sim.stopObserve

      override def abortObserve: F[Unit] = sim.abortObserve

      override def endObserve: F[Unit] = sim.endObserve

      override def pauseObserve: F[Unit] = sim.pauseObserve

      override def resumePaused(expTime: Time): F[ObserveCommandResult] = sim.resumePaused

      override def stopPaused: F[ObserveCommandResult] = sim.stopPaused

      override def abortPaused: F[ObserveCommandResult] = sim.abortPaused

      override def observeProgress(total: Time, elapsed: ElapsedTime): fs2.Stream[F, Progress] =
        sim.observeCountdown(total, elapsed)

    }

  def south[F[_]: Sync: Timer]: GmosController[F, SouthTypes] = GmosControllerSim[F, SouthTypes]("South")
  def north[F[_]: Sync: Timer]: GmosController[F, NorthTypes] = GmosControllerSim[F, NorthTypes]("North")
}
