// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.web.client.components

import seqexec.web.client.actions.Logout
import seqexec.web.client.actions.OpenLoginBox
import seqexec.web.client.model.ClientStatus
import seqexec.web.client.circuit.SeqexecCircuit
import react.semanticui.collections.menu.Menu
import react.semanticui.elements.button.Button
import react.semanticui.elements.header.Header
import react.semanticui.views.item.Item
import react.semanticui.sizes._
import seqexec.web.client.icons._
import japgolly.scalajs.react.component.Scala.Unmounted
import japgolly.scalajs.react.Callback
import japgolly.scalajs.react.React
import japgolly.scalajs.react.ScalaComponent
import japgolly.scalajs.react.vdom.html_<^._

/**
  * Menu with options
  */
object ControlMenu {

  final case class Props(status: ClientStatus)

  private val soundConnect =
    SeqexecCircuit.connect(SeqexecCircuit.soundSettingReader)

  private val openLogin: Callback =
    SeqexecCircuit.dispatchCB(OpenLoginBox)
  private val logout: Callback =
    SeqexecCircuit.dispatchCB(Logout)

  private def loginButton(enabled: Boolean) =
    Button(Button.props(size     = Medium,
                        onClick  = openLogin,
                        disabled = !enabled,
                        inverted = true),
           "Login")

  private def logoutButton(text: String, enabled: Boolean) =
    Button(Button.props(size     = Medium,
                        onClick  = logout,
                        icon     = IconSignOut,
                        disabled = !enabled,
                        inverted = true),
           text)

  private val component = ScalaComponent
    .builder[Props]("SeqexecTopMenu")
    .stateless
    .render_P { p =>
      val status = p.status
      Menu(Menu.props(secondary = true, className = "right"),
        status.u
          .map { user =>
            React.Fragment(
              Header(Header.props(className = "item", clazz = SeqexecStyles.notInMobile),
                user.displayName
              ),
              Header(Header.props(className = "item", clazz = SeqexecStyles.onlyMobile),
                // Ideally we'd do this with css text-overflow but it is not
                // working properly inside a header item, let's abbreviate in code
                user.displayName
                  .split("\\s")
                  .headOption
                  .map(_.substring(0, 10) + "...")
                  .getOrElse[String]("")
              ),
              Item(Item.props(clazz = SeqexecStyles.notInMobile),
                   soundConnect(x => SoundControl(SoundControl.Props(x()))),
                   logoutButton("Logout", status.isConnected)),
              Item(Item.props(clazz = SeqexecStyles.onlyMobile),
                   logoutButton("", status.isConnected))
            )
          }
          .getOrElse {
            Item(soundConnect(x => SoundControl(SoundControl.Props(x()))),
                 loginButton(status.isConnected)): VdomNode
          }
      )
    }
    .build

  def apply(u: ClientStatus): Unmounted[Props, Unit, Unit] =
    component(Props(u))
}
