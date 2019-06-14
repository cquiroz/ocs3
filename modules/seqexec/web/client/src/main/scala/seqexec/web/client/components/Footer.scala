// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.web.client.components

import gem.enum.Site
import japgolly.scalajs.react._
import japgolly.scalajs.react.extra.router.RouterCtl
import japgolly.scalajs.react.vdom.html_<^._
import japgolly.scalajs.react.component.Scala.Unmounted
import japgolly.scalajs.react.Reusability
import react.semanticui.elements.header.Header
import react.semanticui.collections.menu.Menu
import seqexec.web.client.circuit.SeqexecCircuit
import seqexec.web.client.model.Pages._
import seqexec.web.client.OcsBuildInfo
import seqexec.web.client.reusability._

/**
  * Component for the bar at the top of the page
  */
object Footer {
  final case class Props(router: RouterCtl[SeqexecPages], site: Site)

  implicit val propsReuse: Reusability[Props] = Reusability.by(_.site)

  private val userConnect = SeqexecCircuit.connect(SeqexecCircuit.statusReader)

  private val component = ScalaComponent
    .builder[Props]("SeqexecAppBar")
    .stateless
    .render_P(
      p =>
        Menu(Menu.props(inverted = true, className = "footer"),
          Header(Header.props(as = "a", className = "item", clazz = SeqexecStyles.notInMobile),
            s"Seqexec - ${p.site.shortName}"
          ),
          Header(Header.props(as = "a", className = "item", clazz = SeqexecStyles.onlyMobile),
            p.site.shortName
          ),
          Header(Header.props(className = "item", clazz = SeqexecStyles.notInMobile, sub = true),
            OcsBuildInfo.version
          ),
          userConnect(FooterStatus.apply)
        )
    )
    .configure(Reusability.shouldComponentUpdate)
    .build

  def apply(p: Props): Unmounted[Props, Unit, Unit] = component(p)
}
