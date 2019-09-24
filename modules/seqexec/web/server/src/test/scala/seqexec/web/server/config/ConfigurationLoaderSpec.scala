// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.web.server.config

// import cats.effect.{ ContextShift, IO, Timer }
// import fs2.concurrent.Topic
// import fs2.Stream
// import giapi.client.GiapiStatusDb
// import org.http4s._
// import org.http4s.Uri.uri
// import seqexec.model.events._
// import seqexec.server.tcs.GuideConfigDb
// import seqexec.web.server.security.{AuthenticationConfig, AuthenticationService, LDAPConfig}
// import squants.time._
// import scala.concurrent.ExecutionContext
import cats.tests.CatsSuite
import cats.effect.IO
import gem.enum.Site
import pureconfig._
import pureconfig.generic.auto._
import pureconfig.module.catseffect._
import pureconfig.module.http4s._
import scala.concurrent.duration._
import java.nio.file.Paths
import org.http4s.Uri._

class ConfigurationLoaderSpec extends CatsSuite {
// ConfigSource.resources("app.conf").at("web-server").loadF[IO, WebServerConfiguration].unsafeRunSync
  val gcal =
    SmartGcalConfiguration("gsodbtest.gemini.edu", Paths.get("/tmp/smartgcal"))
  val tls  = TLSConfig(Paths.get("file.jks"), "key", "cert")
  val auth  = AuthenticationConfig(2.hour, "SeqexecToken", "somekey", false, List(uri("ldap://sbfdc-wv1.gemini.edu:3268")))
  val ws  = WebServerConfiguration("0.0.0.0", 7070, 7071, "localhost", Some(tls))
  val ref = SeqexecConfiguration(Site.GS, Mode.Development, null, ws, gcal, auth)

  test("read config") {
    println(
      ConfigSource
        .resources("app.conf")
        .loadF[IO, SeqexecConfiguration]
        .unsafeRunSync
    )
    assert(
      ConfigSource
        .resources("app.conf")
        .loadF[IO, SeqexecConfiguration]
        .unsafeRunSync === ref
    )
  }

  // implicit val ioContextShift: ContextShift[IO] =
  //   IO.contextShift(ExecutionContext.global)
  //
  // implicit val ioTimer: Timer[IO] =
  //   IO.timer(ExecutionContext.global)

}
