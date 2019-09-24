// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.web.server.config

import cats.Eq
import cats.implicits._
import gem.enum.Site
import gem.util.Enumerated
import java.nio.file.Path
import seqexec.server.config.ServerConfiguration

final case class SmartGcalConfiguration(
  smartGCalHost: String,
  smartGCalDir:  Path
)

object SmartGcalConfiguration {
  implicit val eqSmartGcalConfiguration: Eq[SmartGcalConfiguration] =
    Eq.by(x => (x.smartGCalHost, x.smartGCalDir))
}

sealed trait Mode extends Product with Serializable

object Mode {
  case object Production extends Mode
  case object Development extends Mode

  implicit val ModeEnumerated: Enumerated[Mode] =
    Enumerated.of(Production, Development)

}

final case class SeqexecConfiguration(
  site:      Site,
  mode:      Mode,
  server: ServerConfiguration,
  webServer: WebServerConfiguration,
  smartGcal: SmartGcalConfiguration,
  authentication: AuthenticationConfig
)

object SeqexecConfiguration {
  implicit val eqSeqexecConfiguration: Eq[SeqexecConfiguration] =
    Eq.by(x => (x.site, x.mode, x.server, x.webServer, x.smartGcal, x.authentication))
}
