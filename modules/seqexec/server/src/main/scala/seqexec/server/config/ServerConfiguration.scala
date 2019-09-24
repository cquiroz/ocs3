// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server.config

import cats.Eq
import cats.implicits._
import gem.enum.Site
import java.time.LocalDate
import org.http4s.Uri
import scala.concurrent.duration.Duration
import shapeless.tag.@@

trait GpiSettings
trait GhostSettings

final case class SystemsControlConfiguration(
  altairControl:           ControlStrategy,
  gemsControl:             ControlStrategy,
  dhsControl:              ControlStrategy,
  f2Control:               ControlStrategy,
  gcalControl:             ControlStrategy,
  gmosControl:             ControlStrategy,
  gnirsControl:            ControlStrategy,
  gpiControl:              ControlStrategy,
  gpiGdsControl:           ControlStrategy,
  ghostControl:            ControlStrategy,
  ghostGdsControl:         ControlStrategy,
  gsaoiControl:            ControlStrategy,
  gwsControl:              ControlStrategy,
  nifsControl:             ControlStrategy,
  niriControl:             ControlStrategy,
  tcsControl:              ControlStrategy
)

object SystemsControlConfiguration {
  implicit val eqSystemsControl: Eq[SystemsControlConfiguration] =
    Eq.by(x => (x.altairControl, x.gemsControl, x.dhsControl, x.f2Control, x.gcalControl, x.ghostControl, x.gmosControl, x.gnirsControl, x.gpiControl, x.gpiGdsControl, x.ghostGdsControl, x.gsaoiControl, x.gwsControl, x.nifsControl, x.niriControl, x.tcsControl))

}

final case class ServerConfiguration(site:         Site,
                          odbHost:                 String,
                          date:                    LocalDate,
                          dhsURI:                  Uri,
                          systemsControl:          SystemsControlConfiguration,
                          odbNotifications:        Boolean,
                          instForceError:          Boolean,
                          failAt:                  Int,
                          odbQueuePollingInterval: Duration,
                          gpiUrl:                  Uri @@ GpiSettings,
                          ghostUrl:                Uri @@ GhostSettings,
                          gpiGDS:                  Uri @@ GpiSettings,
                          ghostGDS:                Uri @@ GhostSettings)
                          // tops:                    String,
                          // caAddrList:              String,
                          // ioTimeout:               Duration)

object ServerConfiguration {
  private implicit val localDateEq: Eq[LocalDate] = Eq.by(_.toEpochDay)
  private implicit def taggedUriEq[A]: Eq[Uri @@ A] = Eq.by(x => x: Uri)

  implicit val eqServerConfiguration: Eq[ServerConfiguration] =
    Eq.by(x => (x.site, x.odbHost, x.date, x.dhsURI, x.systemsControl, x.odbNotifications, x.instForceError, x.failAt, x.odbQueuePollingInterval, x.gpiUrl, x.ghostUrl, x.gpiGDS, x.ghostGDS))

}
