// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server.ghost

import cats.implicits._
import cats.effect.Sync
import giapi.client.commands.Configuration
import giapi.client.ghost.GhostClient
import io.chrisdavenport.log4cats.Logger
import seqexec.server.keywords.GdsClient
import seqexec.server.GiapiInstrumentController
import seqexec.server.AbstractGiapiInstrumentController

trait GhostController[F[_]] extends GiapiInstrumentController[F, GhostConfig] {
  def gdsClient: GdsClient[F]
}

object GhostController {
  def apply[F[_]: Sync: Logger](client: GhostClient[F], gds: GdsClient[F]): GhostController[F] =
    new AbstractGiapiInstrumentController[F, GhostConfig, GhostClient[F]](client) with GhostController[F] {
      override val gdsClient: GdsClient[F] = gds

      override val name = "GHOST"

      override def configuration(config: GhostConfig): F[Configuration] = config.configuration.pure[F]
    }
}
