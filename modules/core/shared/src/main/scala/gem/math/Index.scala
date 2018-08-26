// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem.math

import cats.{ Order, Show, Semigroup }
import cats.instances.short._
import mouse.boolean._
import gem.parser.MiscParsers
import gem.syntax.all._
import monocle.Prism

/** A positive, non-zero value for numbered identifiers. */
sealed abstract case class Index(toShort: Short)
object Index extends IndexOptics {

  val One: Index =
    fromShort.unsafeGet(1)

  implicit val OrderIndex: Order[Index] =
    Order.by(_.toShort)

  implicit val OrderingIndex: scala.math.Ordering[Index] =
    OrderIndex.toOrdering

  implicit val showIndex: Show[Index] =
    Show.fromToString

  implicit val indexAdditionSemigroup: Semigroup[Index] = new Semigroup[Index] {
    def combine(x: Index, y: Index): Index =
      fromShort.unsafeGet((x.toShort + y.toShort).toShort)
  }
}

trait IndexOptics {

  /** @group Optics */
  val fromShort: Prism[Short, Index] =
    Prism((i: Short) => (i > 0) option new Index(i) {})(_.toShort)

  /** @group Optics */
  val fromString: Prism[String, Index] =
    Prism(MiscParsers.index.parseExact)(_.toShort.toString)

}
