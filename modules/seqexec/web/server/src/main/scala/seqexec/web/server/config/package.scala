package seqexec.web.server

import cats.Eq
import cats.implicits._
import gem.enum.Site
import java.nio.file.Path
import pureconfig._
import pureconfig.error._
import pureconfig.generic.ProductHint

package config {
  final case class SiteValueUnknown(site: String) extends FailureReason {
    def description: String = s"site '$site' invalid"
  }
  final case class ModeValueUnknown(site: String) extends FailureReason {
    def description: String = s"mode '$site' invalid"
  }
}

package object config {
  implicit val pathEq: Eq[Path] = Eq.fromUniversalEquals

  implicit val siteReader = ConfigReader.fromCursor[Site]{ cf =>
    cf.asString.flatMap {
      case "GS" => Site.GS.asRight
      case "GN" => Site.GN.asRight
      case s => cf.failed(SiteValueUnknown(s))
    }
  }

  implicit val modeReader = ConfigReader.fromCursor[Mode]{ cf =>
    cf.asString.flatMap {
      case "production" => Mode.Production.asRight
      case "dev" => Mode.Development.asRight
      case s => cf.failed(ModeValueUnknown(s))
    }
  }

  implicit val webServerConfigurationHint = ProductHint[WebServerConfiguration](ConfigFieldMapping(KebabCase, KebabCase))
  implicit val smartGcalConfigurationHint = ProductHint[SmartGcalConfiguration](ConfigFieldMapping(KebabCase, KebabCase))
  implicit val tlsInfoHint = ProductHint[TLSConfig](ConfigFieldMapping(KebabCase, KebabCase))
  implicit val authenticationConfigHint = ProductHint[AuthenticationConfig](ConfigFieldMapping(KebabCase, KebabCase))
}
