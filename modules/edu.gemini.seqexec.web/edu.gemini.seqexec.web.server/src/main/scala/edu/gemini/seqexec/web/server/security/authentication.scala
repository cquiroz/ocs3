package edu.gemini.seqexec.web.server.security

import edu.gemini.seqexec.web.server.security.AuthenticationService.AuthResult

import upickle.default._
import pdi.jwt.{Jwt, JwtAlgorithm, JwtHeader, JwtClaim, JwtOptions}

import scala.annotation.tailrec
import scalaz.{-\/, \/, \/-}

sealed trait AuthenticationFailure
case class UserNotFound(user: String) extends AuthenticationFailure
case class BadCredentials(user: String) extends AuthenticationFailure
case object NoAuthenticator extends AuthenticationFailure
case class GenericFailure(msg: String) extends AuthenticationFailure

case class UserDetails(username: String, displayName: String)

trait AuthenticationService {
  def authenticateUser(username: String, password: String): AuthResult
}

object AuthenticationService {
  type AuthResult = AuthenticationFailure \/ UserDetails

  // TODO externalize
  val sessionTimeout = 8 * 3600
  val onSSL = false

  // Allows calling authenticate on a list of authenticator, stopping at the first
  // that succeeds
  implicit class ComposedAuth(val s: List[AuthenticationService]) extends AnyVal {

    def authenticateUser(username: String, password: String): AuthResult = {
      @tailrec
      def go(l: List[AuthenticationService]): AuthResult = l match {
        case Nil      => -\/(NoAuthenticator)
        case x :: Nil => x.authenticateUser(username, password)
        case x :: xs  => x.authenticateUser(username, password) match {
            case u @ \/-(_) => u
            case -\/(e)     => go(xs)
          }
      }
      go(s)
    }
  }

  def buildToken(u: UserDetails): String = {
    // Given that only this server will need the key we can just use HMAC. 512-bit is the max key size allowed
    Jwt.encode(JwtClaim(write(u)).issuedNow.expiresIn(3600), "secretkey", JwtAlgorithm.HmacSHA256)
  }
}