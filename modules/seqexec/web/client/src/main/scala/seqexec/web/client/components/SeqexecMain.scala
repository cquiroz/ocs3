// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.web.client.components

import cats.implicits._
import diode.react.ReactPot._
import japgolly.scalajs.react.vdom.html_<^._
import japgolly.scalajs.react.extra.router._
import japgolly.scalajs.react.ScalaComponent
import japgolly.scalajs.react.React
import japgolly.scalajs.react.component.Scala.Unmounted
import japgolly.scalajs.react.Reusability
import gem.enum.Site
import react.common.implicits._
import react.semanticui.elements.divider._
import react.semanticui.collections.grid._
import react.semanticui.widths._
import react.common._
import react.common.implicits._
import seqexec.web.client.circuit.SeqexecCircuit
import seqexec.web.client.model.Pages._
import seqexec.web.client.components.tabs.TabsArea
import seqexec.web.client.model.WebSocketConnection
import seqexec.web.client.reusability._

object AppTitle {
  final case class Props(site: Site, ws: WebSocketConnection)

  implicit val propsReuse: Reusability[Props] = Reusability.derive[Props]

  private val component = ScalaComponent
    .builder[Props]("SeqexecTitle")
    .stateless
    .render_P(p =>
      Divider(Divider.props(as = "h4", horizontal = true,
        clazz = Css("header") |+| SeqexecStyles.titleRow |+| SeqexecStyles.notInMobile),
        s"Seqexec ${p.site.shortName}",
        p.ws.ws.renderPending(
          _ =>
            <.div(
              SeqexecStyles.errorText,
              SeqexecStyles.blinking,
              "Connection lost"
          ))
    ))
    .configure(Reusability.shouldComponentUpdate)
    .build

  def apply(p: Props): Unmounted[Props, Unit, Unit] = component(p)
}

object SeqexecMain {
  final case class Props(site: Site, ctl: RouterCtl[SeqexecPages])

  implicit val propsReuse: Reusability[Props] = Reusability.by(_.site)

  private val lbConnect  = SeqexecCircuit.connect(_.uiModel.loginBox)
  private val logConnect = SeqexecCircuit.connect(_.uiModel.globalLog)
  private val userNotificationConnect = SeqexecCircuit.connect(_.uiModel.notification)
  private val headerSideBarConnect = SeqexecCircuit.connect(SeqexecCircuit.headerSideBarReader)
  private val wsConnect = SeqexecCircuit.connect(_.ws)

  private val component = ScalaComponent
    .builder[Props]("SeqexecUI")
    .stateless
    .render_P(p =>
      React.Fragment(
        Grid(Grid.props(padded = GridPadded.Horizontally),
          GridRow(GridRow.props(
            clazz = SeqexecStyles.shorterRow)),
          wsConnect(ws => AppTitle(AppTitle.Props(p.site, ws()))),
          GridRow(GridRow.props(
            clazz = SeqexecStyles.shorterRow |+| SeqexecStyles.queueAreaRow),
            GridColumn(GridColumn.props(
              mobile = Sixteen,
              tablet = Ten,
              computer = Ten,
              clazz = SeqexecStyles.queueArea),
              SessionQueueTableSection(p.ctl)
            ),
            GridColumn(GridColumn.props(
              tablet = Six,
              computer = Six,
              only = GridOnly.Tablet,
              clazz = SeqexecStyles.headerSideBarArea),
              headerSideBarConnect(HeadersSideBar.apply)
            )
          ),
          GridRow(GridRow.props(
            clazz = SeqexecStyles.shorterRow),
            TabsArea(TabsArea.Props(p.ctl, p.site))
          ),
          GridRow(GridRow.props(
            clazz = SeqexecStyles.logArea),
            logConnect(l => LogArea(p.site, l()))
          )
        ),
        lbConnect(p => LoginBox(p())),
        userNotificationConnect(p =>
          UserNotificationBox(UserNotificationBox.Props(p()))),
        Footer(Footer.Props(p.ctl, p.site))
    ))
    .configure(Reusability.shouldComponentUpdate)
    .build

  def apply(site: Site, ctl: RouterCtl[SeqexecPages]): Unmounted[Props, Unit, Unit] = component(Props(site, ctl))
}
