package edu.gemini.seqexec.web.client.components

import diode.FastEq
import diode.data.Pot
import diode.react.ReactPot._
import diode.react.ModelProxy
import edu.gemini.seqexec.web.client.model.{SearchSequence, SeqexecCircuit}
import edu.gemini.seqexec.web.client.semanticui.SemanticUI._
import edu.gemini.seqexec.web.client.semanticui.elements.icon.Icon
import edu.gemini.seqexec.web.common.{SeqexecQueue, Sequence}
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.prefix_<^._
import org.scalajs.dom.ext.KeyCode


object SequenceSearchResults {
  implicit object ExtDataEq extends FastEq[Pot[List[Sequence]]] {
    override def eqv(a: Pot[List[Sequence]], b: Pot[List[Sequence]]): Boolean = a.state == b.state
  }

  val component = ReactComponentB[Unit]("SequenceSearchResult")
    .stateless
    .render_P(p =>
      <.div(
        ^.cls := "six wide column",
        SeqexecCircuit.connect(_.searchResults)(LoadingIndicator("Searching...", _)),
        <.div(
          ^.cls := "ui top attached segment header",
          "Found "// + p().map(_.size).getOrElse(0)
        ),
        <.div(
          ^.cls := "ui scroll pane attached segment",
          <.table(
            ^.cls := "ui selectable compact table unstackable",
            <.thead(
              <.tr(
                <.th("Obs ID"),
                <.th("Instrument"),
                <.th("\u00a0")
              )
            ),
            <.tbody(/*
              p().renderReady(s => s.zipWithIndex.collect { case (u, i) =>
                  <.tr(
                    ^.key := i,
                    <.td(
                      ^.cls := "collapsing",
                      u.id
                    ),
                    <.td("GPI"),
                    <.td(
                      ^.cls := "collapsing",
                      <.button(
                        ^.cls := "circular ui icon button",
                        Icon("plus")
                      )
                    )
                  )
                }
              )*/
            )
          )
        )
      )
    )
    .buildU

  def apply() = component()
}
object SequenceSearch {
  case class Props(searchResults: ModelProxy[Pot[List[Sequence]]])

  case class State(searchText: String, searching: Boolean)

  class Backend($: BackendScope[Props, State]) {
    def onEnter(e: ReactKeyboardEventI): Callback = Callback.ifTrue(e.charCode == KeyCode.Enter, search)

    def searchingIndicator: Callback =
      $.modState(_.copy(searching = true))

    def requestSearch: Callback =
      $.props.zip($.state) >>= {case (p, s) => p.searchResults.dispatch(SearchSequence(s.searchText))}

    def search: Callback = searchingIndicator >> requestSearch

    def onChange(e: ReactEventI): Callback =
      // For some reason the simple call $.modState(_.copy(searchText = e.target.value)) gives an NPE on e.target
      // Possibly a react.js fault, but the code below does the trick
      CallbackTo.pure(e.target.value) >>= {e => $.modState(_.copy(searchText = e))}

    def render(p: Props, s: State) =
      <.div(
        ^.cls := "ui right aligned category search dropdown item",
        ^.classSet(
          "loading" -> s.searching
        ),
        <.div(
          ^.cls := "ui transparent icon input",
          <.input(
            ^.cls := "prompt",
            ^.`type` := "text",
            ^.placeholder := "Search...",
            ^.onKeyPress ==> onEnter,
            ^.onChange ==> onChange,
            ^.value := s.searchText
          ),
          Icon(Icon.Props("search", link = true, onClick = search))
        ),
        <.div(
          ^.cls := "menu"
          // TODO Add elements of the queue as <div class="item">text</div>
        )
      )
  }

  val component = ReactComponentB[Props]("SequenceSearch")
    .initialState(State("", searching = false))
    .renderBackend[Backend]
    .componentDidMount(s =>
      Callback {
        // Enable menu on Semantic UI
        import org.querki.jquery.$

        $(ReactDOM.findDOMNode(s)).dropdown()
      }
    )
    .build

  def apply(searchResults: ModelProxy[Pot[List[Sequence]]]) = component(Props(searchResults))
}
