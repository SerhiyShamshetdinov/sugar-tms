package sands.sugar.tms

import sands.sugar.tms.TransparentMonads._

import scala.language.{higherKinds, implicitConversions}
import scala.util.{Failure, Try}

/*
 * Created by Serhiy Shamshetdinov
 * at 23.06.2021 19:17
 */

object Main extends App {

  case class User(id: String)
  case class UserBalance(amount: BigDecimal, currencyId: String)
  case class BalanceInfoRequest(currencyId: String)
  case class BalanceInfoResponse(currencyId: String, currencyBalance: BigDecimal)

  def sessionIsActive(sessionId: String): Try[Boolean] = Try(sessionId == "good session")
  def getUser(sessionId: String): Try[User] = Try(User("user-id"))
  def getUserBalance(userId: String): Try[UserBalance] = Try(UserBalance(10, "USD"))
  def getExchangeRate(fromCurrencyId: String, toCurrencyId: String): Try[BigDecimal] = Try(1.1)
  def saveAudit(sessionId: String, request: BalanceInfoRequest, response: BalanceInfoResponse): Try[Unit] = Try({})
  def raiseError(exception: Exception): Failure[Nothing] = Failure(exception)

  @tmsOptions("no debug; no trace")
  def getBalanceInfoResponse(sessionId: String, request: BalanceInfoRequest): Try[BalanceInfoResponse] =
    monadicFlowControl[Try[BalanceInfoResponse]] {
      val user = if (sessionIsActive(sessionId)) getUser(sessionId) else raiseError(new Exception("Bad session"))
      val userBalance = getUserBalance(user.id)
      val currencyBalance = if (userBalance.currencyId == request.currencyId)
        userBalance.amount
      else
        userBalance.amount * getExchangeRate(userBalance.currencyId, request.currencyId)

      val response = BalanceInfoResponse(request.currencyId, currencyBalance)

      saveAudit(sessionId, request, response)

      response
    }

  println(getBalanceInfoResponse("good session", BalanceInfoRequest("EUR")))
  println(getBalanceInfoResponse("good session", BalanceInfoRequest("USD")))
  println(getBalanceInfoResponse("session timeout", BalanceInfoRequest("USD")))
}
