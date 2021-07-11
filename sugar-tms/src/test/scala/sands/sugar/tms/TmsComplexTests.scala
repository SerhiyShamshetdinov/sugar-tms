/*
 * Transparent Monads syntax and Monadic Flow Control interpretation
 *
 * Copyright (c) 2021 Serhiy Shamshetdinov (Kyiv, Ukraine)
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership and used works.
 */

package sands.sugar.tms

import sands.sugar.tms.TmsForTestOptions._
import sands.sugar.tms.TransparentMonads._

import scala.Seq
import scala.tools.reflect.ToolBoxError
import scala.util.{Failure, Success, Try}

/*
 * Created by Serhiy Shamshetdinov
 * at 18.02.2021 18:38
 */

object TmsComplexTests {
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
}

class TmsComplexTests extends TmsTestBase {
  import TmsComplexTests._

  complexTests()
  testWithForsStackWithPreEvaluationVariants(complexToolboxTests)

  @tmsOptions("ND;NT")
  def complexTests(): Unit = {

    behavior of "tms/mfc macros in complex tests: NON-TOOLBOX"

    it should "process mfc getBalanceInfoResponse test" in {

      def getBalanceInfoResponse(sessionId: String, request: BalanceInfoRequest): Try[BalanceInfoResponse] = {
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
        // Typechecked code with dropped packages:
        //[debug] * tms INPUT code.tree:
        //{
        //  val user = if (tts1[Try, Boolean](sessionIsActive(sessionId)))
        //    getUser(sessionId)
        //  else
        //    raiseError(new Exception("Bad session"));
        //  val userBalance = getUserBalance(tts1[Try, User](user).id);
        //  val currencyBalance = if (tts1[Try, UserBalance](userBalance).currencyId.==(request.currencyId))
        //    tts1[Try, UserBalance](userBalance).amount
        //  else
        //    tts1[Try, UserBalance](userBalance).amount.*(tts1[Try, BigDecimal](getExchangeRate(tts1[Try, UserBalance](userBalance).currencyId, request.currencyId)));
        //  val response = BalanceInfoResponse.apply(request.currencyId, currencyBalance);
        //  saveAudit(sessionId, request, response);
        //  response
        //}

        //[debug] * tms extracted fors code view:
        //{
        //  for {
        //    valueOfTry$macro$1 <- sessionIsActive(sessionId)
        //    user = if (valueOfTry$macro$1)
        //      getUser(sessionId)
        //    else
        //      raiseError(new Exception("Bad session"))
        //    userValue$macro$2 <- user
        //    userBalance = getUserBalance(userValue$macro$2.id)
        //    userBalanceValue$macro$3 <- userBalance
        //    valueOfTry$macro$4 <- getExchangeRate(userBalanceValue$macro$3.currencyId, request.currencyId)
        //    currencyBalance = if (userBalanceValue$macro$3.currencyId.$eq$eq(request.currencyId))
        //      userBalanceValue$macro$3.amount
        //    else
        //      userBalanceValue$macro$3.amount.$times(valueOfTry$macro$4)
        //    response = BalanceInfoResponse.apply(request.currencyId, currencyBalance)
        //    wcMf$macro$5 <- saveAudit(sessionId, request, response)
        //  } yield {
        //    response
        //  }
        //}
        
        //[debug] * tms postprocessed fors code view:
        //{
        //  for {
        //    valueOfTry$macro$1 <- sessionIsActive(sessionId)
        //    userValue$macro$2 <- if (valueOfTry$macro$1)
        //      getUser(sessionId)
        //    else
        //      raiseError(new Exception("Bad session"))
        //    userBalanceValue$macro$3 <- getUserBalance(userValue$macro$2.id)
        //    valueOfTry$macro$4 <- getExchangeRate(userBalanceValue$macro$3.currencyId, request.currencyId)
        //    currencyBalance = if (userBalanceValue$macro$3.currencyId.$eq$eq(request.currencyId))
        //      userBalanceValue$macro$3.amount
        //    else
        //      userBalanceValue$macro$3.amount.$times(valueOfTry$macro$4)
        //    response = BalanceInfoResponse.apply(request.currencyId, currencyBalance)
        //    wcMf$macro$5 <- saveAudit(sessionId, request, response)
        //  } yield {
        //    response
        //  }
        //}
      }

      getBalanceInfoResponse("good session", BalanceInfoRequest("EUR")) shouldBe Success(BalanceInfoResponse("EUR", 11))
      getBalanceInfoResponse("good session", BalanceInfoRequest("USD")) shouldBe Success(BalanceInfoResponse("USD", 10))
      getBalanceInfoResponse("session timeout", BalanceInfoRequest("USD")).failed.get.getMessage shouldBe "Bad session"
    }
  }

  def complexToolboxTests(tmsOptions: Seq[String]): Unit = {
    val options = literalOptions(tmsOptions)

    behavior of "tms/mfc macros in complex toolbox tests with TmsOptions" + options

    it should "pass mfc macro getBalanceInfoResponse test" in new ToolboxTest {
      val testCode: String =
        s"""$ToolboxTestImports
           |import sands.sugar.tms.TmsComplexTests._
           |
           |def getBalanceInfoResponse(sessionId: String, request: BalanceInfoRequest): Try[BalanceInfoResponse] =
           |  monadicFlowControlFor[Try[BalanceInfoResponse]]$options {
           |    val user = if (sessionIsActive(sessionId)) getUser(sessionId) else raiseError(new Exception("Bad session"))
           |    val userBalance = getUserBalance(user.id)
           |    val currencyBalance = if (userBalance.currencyId == request.currencyId)
           |      userBalance.amount
           |    else
           |      userBalance.amount * getExchangeRate(userBalance.currencyId, request.currencyId)
           |
           |    val response = BalanceInfoResponse(request.currencyId, currencyBalance)
           |
           |    saveAudit(sessionId, request, response)
           |
           |    response
           |  }
           |
           |(getBalanceInfoResponse("good session", BalanceInfoRequest("EUR")),
           | getBalanceInfoResponse("good session", BalanceInfoRequest("USD")),
           | getBalanceInfoResponse("session timeout", BalanceInfoRequest("USD")))
           |
           |""".stripMargin

      val result = evaluateCode(testCode).asInstanceOf[(Try[BalanceInfoResponse], Try[BalanceInfoResponse], Try[BalanceInfoResponse])]
      result._1 shouldBe Success(BalanceInfoResponse("EUR", 11))
      result._2 shouldBe Success(BalanceInfoResponse("USD", 10))
      result._3.failed.get.getMessage shouldBe "Bad session"
    }

    it should "fail tms macro getBalanceInfoResponse test" in new ToolboxTest {
      val testCode: String =
        s"""$ToolboxTestImports
           |import sands.sugar.tms.TmsComplexTests._
           |
           |def getBalanceInfoResponse(sessionId: String, request: BalanceInfoRequest): Try[BalanceInfoResponse] =
           |  transparentMonadsFor[Try[BalanceInfoResponse]]$options {
           |    val user = if (sessionIsActive(sessionId)) getUser(sessionId) else raiseError(new Exception("Bad session"))
           |    val userBalance = getUserBalance(user.id)
           |    val currencyBalance = if (userBalance.currencyId == request.currencyId)
           |      userBalance.amount
           |    else
           |      userBalance.amount * getExchangeRate(userBalance.currencyId, request.currencyId)
           |
           |    val response = BalanceInfoResponse(request.currencyId, currencyBalance)
           |
           |    saveAudit(sessionId, request, response)
           |
           |    response
           |  }
           |
           |getBalanceInfoResponse("good session", BalanceInfoRequest("EUR"))
           |
           |""".stripMargin

      Try(evaluateCode(testCode)) should matchPattern {
        case Failure(ToolBoxError(msg, _)) if msg.startsWith(
          "reflective typecheck has failed: locally defined (in tmsCode) 'value user' may not be used") =>
      }
    }

    it should "pass tms macro addValue test" in new ToolboxTest {
      val testCode: String =
        s"""$ToolboxTestImports
           |
           |def addValue(list: List[Option[Int]], value: Long): List[Option[Long]] =
           |  tmsFor[List[Option[Long]]]$options {
           |    value + list // for `list + value` result will be the same
           |  }
           |
           |addValue(List[Option[Int]](None, Some(1)), 5)
           |
           |""".stripMargin

      evaluateCode(testCode) shouldBe List(None, Some(6))
    }

    it should "pass mfc macro patternTimes test" in new ToolboxTest {
      val testCode: String =
        s"""$ToolboxTestImports
           |
           |def log(v: Any): Unit = {}
           |
           |@tmsOptions$options
           |def patternTimes(n: Int, m: Int, pattern: Try[String]): Try[String] = mfc[Try[String]] {
           |  val times = Try(n/m)     // Monadic Flow Type is Try
           |  log(s"$$n/$$m=")         // the group of 2 non Monadic Flow Type expressions:
           |  log(times)
           |  val len = pattern.length // non Monadic Flow Type stable val
           |
           |  log(len)                 // yield starts here
           |  pattern * times
           |}
           |
           |Seq(
           |  patternTimes(5, 2, Try("String")),
           |  patternTimes(5, 0, Try("String")),
           |  patternTimes(5, 2, scala.util.Failure(new Exception("Test")))
           |)
           |""".stripMargin

      val resSeq = evaluateCode(testCode).asInstanceOf[Seq[Try[String]]]

      resSeq.head                     shouldBe Try("StringString")
      resSeq(1).failed.get.getMessage shouldBe "/ by zero"
      resSeq(2).failed.get.getMessage shouldBe "Test"
    }
  }
}
