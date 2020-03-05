// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server.ghost

import cats.kernel.laws.discipline._
import cats.tests.CatsSuite
import giapi.client.GiapiConfig._
import gem.enum.GiapiStatusApply._
import gsp.math.Coordinates
import gsp.math.RightAscension
import gsp.math.Declination
import org.scalatest.EitherValues
import scala.concurrent.duration._

/**
  * Tests GHOST Config typeclasses
  */
final class GhostSpec extends CatsSuite with GhostArbitraries with EitherValues {
  checkAll("Eq[GHOSTConfig]", EqTests[GhostConfig].eqv)
  val dec = Declination.fromRadians(1.0).getOrElse(Declination.Max)
  val ra = RightAscension.fromRadians(2.0)
  val coord = Coordinates(ra, dec)

  test("fiber agitator on") {
    val cfg = GhostConfig(none, 1.seconds, FiberAgitator.On, "target".some, Coordinates.Zero.some, none, none, none, none, none, none)
    cfg.toOption.flatMap(_.configuration.value(GhostFiberAgitator.applyItem)) shouldBe Some("1")
  }
  test("fiber agitator off") {
    val cfg = GhostConfig(none, 1.seconds, FiberAgitator.Off, "target".some, Coordinates.Zero.some, none, none, none, none, none, none)
    cfg.toOption.flatMap(_.configuration.value(GhostFiberAgitator.applyItem)) shouldBe Some("0")
  }
  test("sru ifu1 ra/dec") {
    val cfg = GhostConfig(none, 1.seconds, FiberAgitator.Off, "target".some, coord.some, none, none, none, none, none, none)
    println(cfg.right.get.configuration)
    // The items are wrong, they should be in Deg
    cfg.toOption.flatMap(_.configuration.value(GhostSRIFU1CoordsRAHMS.applyItem)) shouldBe Some(doubleConfig.configValue(ra.toAngle.toDoubleDegrees))
    cfg.toOption.flatMap(_.configuration.value(GhostSRIFU1CoordsDecDMS.applyItem)) shouldBe Some(doubleConfig.configValue(dec.toAngle.toDoubleDegrees))
    cfg.toOption.flatMap(_.configuration.value("ghost:cc:cu:ifu1.type")) shouldBe Some(DemandType.DemandRADec.demandType)
    // This is wrong according to the ICD it should be ghost:cc:cu:ifu1.ifu
    cfg.toOption.flatMap(_.configuration.value("ghost:cc:cu:ifu1.bundle")) shouldBe Some(BundleConfig.Standard.configName)
    cfg.toOption.flatMap(_.configuration.value("ghost:cc:cu:ifu1.target")) shouldBe Some(IFUTargetType.Target("Some").targetType)
    // ifu2 not used
    cfg.toOption.flatMap(_.configuration.value("ghost:cc:cu:ifu2.type")) shouldBe Some(DemandType.DemandPark.demandType)
    cfg.toOption.flatMap(_.configuration.value("ghost:cc:cu:ifu2.target")) shouldBe Some(IFUTargetType.NoTarget.targetType)
  }
}
