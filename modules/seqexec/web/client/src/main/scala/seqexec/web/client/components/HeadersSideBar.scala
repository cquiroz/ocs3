// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.web.client.components

import cats.Eq
import cats.implicits._
import cats.Show
import japgolly.scalajs.react.BackendScope
import japgolly.scalajs.react.Callback
import japgolly.scalajs.react.CatsReact._
import japgolly.scalajs.react.extra.StateSnapshot
import japgolly.scalajs.react.extra.TimerSupport
import japgolly.scalajs.react.MonocleReact._
import japgolly.scalajs.react.Reusability
import japgolly.scalajs.react.Reusability
import japgolly.scalajs.react.ScalaComponent
import japgolly.scalajs.react.vdom.html_<^._
import monocle.macros.Lenses
import react.common._
import react.semanticui.collections.form._
import react.semanticui.elements.segment.Segment
import react.semanticui.widths._
import scala.concurrent.duration._
import seqexec.model.enum.CloudCover
import seqexec.model.enum.ImageQuality
import seqexec.model.enum.SkyBackground
import seqexec.model.enum.WaterVapor
import seqexec.model.Observer
import seqexec.model.Operator
import seqexec.web.client.actions._
import seqexec.web.client.circuit._
import seqexec.web.client.components.forms.EnumSelect
import seqexec.web.client.components.forms.FormLabel
import seqexec.web.client.components.forms.InputEV
import seqexec.web.client.reusability._

/**
  * Container for a table with the steps
  */
final case class HeadersSideBar(model: HeaderSideBarFocus) extends ReactProps {
  @inline def render: VdomElement = HeadersSideBar.component(this)

  def canOperate: Boolean = model.status.canOperate
  def selectedObserver: Either[Observer, Either[DayCalObserverFocus, SequenceObserverFocus]] =
    model.observer
}

/**
  * Display to show headers per sequence
  */
object HeadersSideBar {
  implicit val eqHeadersSideBar: Eq[HeadersSideBar]    = Eq.by(_.model)
  implicit val propsReuse: Reusability[HeadersSideBar] = Reusability.byEq

  implicit val showSkyBackground: Show[SkyBackground] =
    Show.show(_.label)

  implicit val showWaterVapor: Show[WaterVapor] =
    Show.show(_.label)

  implicit val showCloudCover: Show[CloudCover] =
    Show.show(_.label)

  implicit val showImageQuality: Show[ImageQuality] =
    Show.show(_.label)

  @Lenses
  final case class State(operator: Option[Operator], observer: Option[Observer])

  object State {
    implicit val equals: Eq[State] = Eq.fromUniversalEquals

    implicit val stateReuse: Reusability[State] = Reusability.derive

  }

  class Backend(val $ : BackendScope[HeadersSideBar, State]) extends TimerSupport {
    private def updateOperator(name: Operator): Callback =
      $.props >>= { p => SeqexecCircuit.dispatchCB(UpdateOperator(name)).when_(p.canOperate) }

    private def updateObserver(name: Observer): Callback =
      $.props >>= { p =>
        (p.selectedObserver match {
          case Right(Right(a)) =>
            SeqexecCircuit.dispatchCB(UpdateObserver(a.obsId, name))
          case Right(Left(_)) =>
            SeqexecCircuit.dispatchCB(UpdateCalTabObserver(name))
          case Left(_) =>
            SeqexecCircuit.dispatchCB(UpdateDefaultObserver(name))
        }).when_(p.canOperate)
      }

    def updateStateOp(value: Option[Operator], cb: Callback = Callback.empty): Callback =
      $.setStateL(State.operator)(value) >> cb

    def updateStateOb(value: Option[Observer], cb: Callback = Callback.empty): Callback =
      $.setStateL(State.observer)(value) >> cb

    def setupTimer: Callback =
      // Every 2 seconds check if the field has changed and submit
      setInterval(submitIfChangedOp *> submitIfChangedOb, 2.second)

    def submitIfChangedOp: Callback =
      ($.state.zip($.props)) >>= {
        case (s, p) =>
          s.operator
            .map(updateOperator)
            .getOrEmpty
            .when_(p.model.operator =!= s.operator)
      }

    def submitIfChangedOb: Callback =
      ($.state.zip($.props)) >>= {
        case (s, p) =>
          p.selectedObserver match {
            case Right(Right(a)) =>
              s.observer
                .map(updateObserver)
                .getOrEmpty
                .when_(a.observer.forall(_.some =!= s.observer))
            case Right(Left(a)) =>
              s.observer
                .map(updateObserver)
                .getOrEmpty
                .when_(a.observer.forall(_.some =!= s.observer))
            case Left(o) =>
              s.observer
                .map(updateObserver)
                .getOrEmpty
                .when_(o.some =!= s.observer)
          }
      }

    def iqChanged(iq: ImageQuality): Callback =
      SeqexecCircuit.dispatchCB(UpdateImageQuality(iq))

    def ccChanged(i: CloudCover): Callback =
      SeqexecCircuit.dispatchCB(UpdateCloudCover(i))

    def sbChanged(sb: SkyBackground): Callback =
      SeqexecCircuit.dispatchCB(UpdateSkyBackground(sb))

    def wvChanged(wv: WaterVapor): Callback =
      SeqexecCircuit.dispatchCB(UpdateWaterVapor(wv))

    def render(p: HeadersSideBar, s: State): VdomNode = {
      val enabled = p.model.status.canOperate
      val operatorEV =
        StateSnapshot[Operator](s.operator.getOrElse(Operator.Zero))(updateStateOp)
      val observerEV =
        StateSnapshot[Observer](s.observer.getOrElse(Observer.Zero))(updateStateOb)
      val instrument = p.selectedObserver
        .map(i => i.fold(_ => "Daycal", _.instrument.show))
        .getOrElse("Default")
      val obsCompleted =
        p.selectedObserver.map(_.fold(_ => false, _.completed)).getOrElse(false)
      val observerField = s"Observer - $instrument"

      Segment(secondary = true, clazz = SeqexecStyles.headerSideBarStyle)(
        Form()(
          FormGroup(widths = Two, clazz = SeqexecStyles.fieldsNoBottom)(
            <.div(
              ^.cls := "eight wide field",
              FormLabel("Operator", Some("operator")),
              InputEV[Operator](
                "operator",
                "operator",
                operatorEV,
                Operator.valueP,
                placeholder = "Operator...",
                disabled    = !enabled,
                onBlur      = _ => submitIfChangedOp
              )
            ),
            <.div(
              ^.cls := "eight wide field",
              FormLabel(observerField, Some("observer")),
              InputEV[Observer](
                "observer",
                "observer",
                observerEV,
                Observer.valueP,
                placeholder = "Observer...",
                disabled    = !enabled || obsCompleted,
                onBlur      = _ => submitIfChangedOb
              )
            )
          ),
          FormGroup(widths = Two, clazz = SeqexecStyles.fieldsNoBottom)(
            EnumSelect[ImageQuality]("Image Quality",
                                     p.model.conditions.iq.some,
                                     "Select",
                                     disabled = !enabled,
                                     iqChanged),
            EnumSelect[CloudCover]("Cloud Cover",
                                   p.model.conditions.cc.some,
                                   "Select",
                                   disabled = !enabled,
                                   ccChanged)
          ),
          FormGroup(widths = Two, clazz = SeqexecStyles.fieldsNoBottom)(
            EnumSelect[WaterVapor]("Water Vapor",
                                   p.model.conditions.wv.some,
                                   "Select",
                                   disabled = !enabled,
                                   wvChanged),
            EnumSelect[SkyBackground]("Sky Background",
                                      p.model.conditions.sb.some,
                                      "Select",
                                      disabled = !enabled,
                                      sbChanged)
          )
        )
      )
    }
  }

  private val component = ScalaComponent
    .builder[HeadersSideBar]("HeadersSideBar")
    .initialState(State(None, None))
    .renderBackend[Backend]
    .configure(TimerSupport.install)
    .componentWillMount(f =>
      f.backend.$.props >>= { p =>
        p.model.operator
          .map(op => f.backend.updateStateOp(Operator(op.value).some))
          .getOrEmpty *>
          (p.selectedObserver match {
            case Right(Right(a)) =>
              f.backend.updateStateOb(a.observer)
            case Right(Left(a)) =>
              f.backend.updateStateOb(a.observer)
            case Left(o) =>
              f.backend.updateStateOb(Observer(o.value).some)
          })
      }
    )
    .componentDidMount(_.backend.setupTimer)
    .componentWillReceiveProps { f =>
      val operator = f.nextProps.model.operator
      val observer = f.nextProps.selectedObserver match {
        case Right(Right(a)) => a.observer
        case Right(Left(a))  => a.observer
        case Left(o)         => o.some
      }
      // Update the operator and observator fields
      Callback.when(
        (operator =!= f.state.operator) && operator.nonEmpty
      )(f.setStateL(State.operator)(operator)) *>
        Callback.when(
          (observer =!= f.state.observer) && observer.nonEmpty
        )(f.setStateL(State.observer)(observer))
    }
    .configure(Reusability.shouldComponentUpdate)
    .build

}
