// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server


package object config {
  implicit class ControlStrategyOps(v: ControlStrategy) {
    val connect: Boolean = v match {
      case ControlStrategy.Simulated => false
      case _         => true
    }
    // If connected, then use real values for keywords
    val realKeywords: Boolean = connect
    val command: Boolean = v match {
      case ControlStrategy.FullControl => true
      case _           => false
    }
  }

}
