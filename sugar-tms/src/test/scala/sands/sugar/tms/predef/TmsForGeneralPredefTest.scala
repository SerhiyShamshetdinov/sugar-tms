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

package sands.sugar.tms.predef

import sands.sugar.tms.TmsForTestOptions._
import sands.sugar.tms.TmsTestBase

import scala.util.{Failure, Success, Try}

/*
 * Created by Serhiy Shamshetdinov
 * at 18.02.2021 16:53
 */

class TmsForGeneralPredefTest extends TmsTestBase {

  generalPredefTests(SimpleForsStackOptions)
  predefComplianceTests(SimpleForsStackOptions)
  noPredefComplianceTests(SimpleForsStackOptions)

  def generalPredefTests(tmsOptions: Seq[String]): Unit = {
    val options = literalOptions(tmsOptions)

    behavior of "tms macros Scala Predef General implicits with TmsOptions" + options

    // Scala Predef implicit classes -----------------------------------------------------

    it should "accept ArrowAssoc[A] ops correctly for explicit tts1 arguments" in new ToolboxTest {
      val testCode: String =
        s"""$ToolboxTestImports
           |
           |tmsFor[Option[(Int, String)]]$options( tts1(Some(1)) -> tts1(Some("2")) )
           |
           |""".stripMargin

      evaluateCode(testCode) shouldBe Some(1 -> "2")
    }

    it should "accept ArrowAssoc[A] ops correctly for explicit tts1 arguments getting _1 of Option[Tuple2]" in new ToolboxTest {
      val testCode: String =
        s"""$ToolboxTestImports
           |
           |tmsFor[Option[Int]]$options( (tts1(Some(1)) → tts1(Some("2")))._1 )
           |
           |""".stripMargin

      evaluateCode(testCode) shouldBe Some(1)
    }

    it should "accept ArrowAssoc[A] ops correctly for explicit tts1 arguments getting _2 of Option[Tuple2]" in new ToolboxTest {
      val testCode: String =
        s"""$ToolboxTestImports
           |
           |tmsFor[Option[String]]$options( (tts1(Some(1)) -> tts1(Some("2")))._2 )
           |
           |""".stripMargin

      evaluateCode(testCode) shouldBe Some("2")
    }

    it should "accept Ensuring[A].ensuring for explicit tts1 argument passing the check" in new ToolboxTest {
      val testCode: String =
        s"""$ToolboxTestImports
           |
           |tmsFor[Try[Int]]$options( tts1(Try(1)).ensuring(_ > 0) )
           |
           |""".stripMargin

      evaluateCode(testCode) shouldBe Success(1)
    }

    it should "accept Ensuring[A].ensuring for explicit tts1 argument failing the check" in new ToolboxTest {
      val testCode: String =
        s"""$ToolboxTestImports
           |
           |tmsFor[Try[Int]]$options( tts1(Try(1)).ensuring(_ < 0) )
           |
           |""".stripMargin

      evaluateCode(testCode) shouldBe a[Failure[_]]
    }

    it should "accept StringFormat[A].formatted for any argument type and use it without tms applying" in new ToolboxTest {
      val testCode: String =
        // mkString is here just to have filled stack to skip warn
        s"""$ToolboxTestImports
           |
           |tmsFor[Try[String]]$options( Try(Some(1).formatted("%3s")).mkString )
           |
           |""".stripMargin

      evaluateCode(testCode) shouldBe Success("Some(1)")
    }

    it should "accept StringFormat[A].formatted for explicit tts1 argument and format inner value" in new ToolboxTest {
      val testCode: String =
        s"""$ToolboxTestImports
           |
           |tmsFor[Option[String]]$options( tts1(Some(1)).formatted("%3s") )
           |
           |""".stripMargin

      evaluateCode(testCode) shouldBe Some("  1")
    }

    it should "repeat Predef.any2stringadd behaviour correctly with explicit any2stringadd call for any.+(s: String) outside of ttsFor macros" in new ToolboxTest {
      val testCode: String =
        s"""$ToolboxTestImports
           |
           |any2stringadd(Some(1)) + ": Some(1) should be prepended (not 1)"
           |
           |""".stripMargin

      evaluateCode(testCode) shouldBe (Some(1) + ": Some(1) should be prepended (not 1)")
    }

    it should "not repeat Predef.any2stringadd behaviour for any.`non +`(s: String) and do ttsFor" in new ToolboxTest {
      val testCode: String =
        s"""$ToolboxTestImports
           |
           |tmsFor[Option[Boolean]]$options(Some("a").equalsIgnoreCase("A"))
           |
           |""".stripMargin

      evaluateCode(testCode) shouldBe Option(true)
    }

    if (ScalaStringVersion < "2.13") {
      it should "accept RichException.getStackTraceString for explicit tts1 argument" in new ToolboxTest {
        val testCode: String =
          s"""$ToolboxTestImports
             |tmsFor[Option[Boolean]]$options {
             |  tts1(Some( new Exception() )).getStackTraceString.endsWith(scala.compat.Platform.EOL)
             |}
             |""".stripMargin

        evaluateCode(testCode) shouldBe Some(true)
      }
    }
  }

  def predefComplianceTests(tmsOptions: Seq[String]): Unit = {
    val options = literalOptions(tmsOptions :+ "Predef Compliance")

    behavior of "tms macros options sensitive behaviour WITH Scala Predef Compliance with TmsOptions" + options

    // .mkString is added to test code just to have build fors stack - to skip warning "NO built fors"

    it should "repeat Predef.any2stringadd behaviour for any.+(s: String) correctly" in new ToolboxTest {
      val testCode: String =
        s"""$ToolboxTestImports
           |
           |tmsFor[Try[String]]$options(Try(Some(1) + ": Some(1) should be prepended (not 1)").mkString)
           |
           |""".stripMargin

      evaluateCode(testCode) shouldBe Try("Some(1): Some(1) should be prepended (not 1)")
    }

    it should "repeat Predef.any2stringadd behaviour for any.+(s: java.lang.String) correctly" in new ToolboxTest {
      val testCode: String =
        s"""$ToolboxTestImports
           |
           |tmsFor[Try[String]]$options(Try(Some(1) + new java.lang.String(": Some(1) should be prepended (not 1)")).mkString)
           |
           |""".stripMargin

      evaluateCode(testCode) shouldBe Try("Some(1): Some(1) should be prepended (not 1)")
    }

    it should "repeat Predef.any2stringadd behaviour for any.+(s: StringAlias) correctly" in new ToolboxTest {
      val testCode: String =
        s"""$ToolboxTestImports
           |
           |type StringAlias = String
           |
           |tmsFor[Try[String]]$options(Try(Some(1) + (": Some(1) should be prepended (not 1)": StringAlias)).mkString)
           |
           |""".stripMargin

      evaluateCode(testCode) shouldBe Try("Some(1): Some(1) should be prepended (not 1)")
    }

    it should "repeat Predef.any2stringadd behaviour for any.$plus(s: String) correctly" in new ToolboxTest {
      val testCode: String =
        s"""$ToolboxTestImports
           |
           |tmsFor[Try[String]]$options(Try(Some(1).$$plus(": Some(1) should be prepended (not 1)")).mkString)
           |
           |""".stripMargin

      evaluateCode(testCode) shouldBe Try("Some(1): Some(1) should be prepended (not 1)")
    }

    it should "repeat Predef.any2stringadd behaviour for ascripted (any: Int).$plus(s: String) correctly" in new ToolboxTest {
      val testCode: String =
        s"""$ToolboxTestImports
           |
           |tmsFor[Try[String]]$options(Try((Some(1): Int) + ": Some(1) should be prepended (not 1)").mkString)
           |
           |""".stripMargin

      evaluateCode(testCode) shouldBe Try("Some(1): Some(1) should be prepended (not 1)")
    }

    it should "repeat Predef.any2stringadd behaviour for ascripted (tts1(any): Int).$plus(s: String) correctly" in new ToolboxTest {
      val testCode: String =
        s"""$ToolboxTestImports
           |
           |tmsFor[Try[String]]$options( Try((tts1[Option, Int](Some(1)): Int) + ": Some(1) should be prepended (not 1)").mkString )
           |
           |""".stripMargin

      evaluateCode(testCode) shouldBe Try("Some(1): Some(1) should be prepended (not 1)")
    }

    it should "repeat Predef.any2stringadd behaviour for repeated any.+(s: String) correctly" in new ToolboxTest {
      val testCode: String =
        s"""$ToolboxTestImports
           |
           |tmsFor[Try[String]]$options(Try(Some(1) + "2" + "3" + Some(4)).mkString)
           |
           |""".stripMargin

      evaluateCode(testCode) shouldBe Try("Some(1)23Some(4)")
    }
  }

  def noPredefComplianceTests(tmsOptions: Seq[String]): Unit = {
    val options = literalOptions(tmsOptions :+ "No Predef Compliance")

    behavior of "tms macros options sensitive behaviour WITHOUT Scala Predef Compliance with TmsOptions" + options

    it should "not repeat Predef.any2stringadd behaviour for any.+(s: String)" in new ToolboxTest {
      val testCode: String =
        s"""$ToolboxTestImports
           |
           |tmsFor[Option[String]]$options(Some(1) + ": 1 should be prepended (not Some(1))")
           |
           |""".stripMargin

      evaluateCode(testCode) shouldBe Some("1: 1 should be prepended (not Some(1))")
    }

    it should "not repeat Predef.any2stringadd behaviour for any.+(s: java.lang.String)" in new ToolboxTest {
      val testCode: String =
        s"""$ToolboxTestImports
           |
           |tmsFor[Option[String]]$options(Some(1) + new java.lang.String(": 1 should be prepended (not Some(1))"))
           |
           |""".stripMargin

      evaluateCode(testCode) shouldBe Some("1: 1 should be prepended (not Some(1))")
    }

    it should "not repeat Predef.any2stringadd behaviour for any.+(s: StringAlias)" in new ToolboxTest {
      val testCode: String =
        s"""$ToolboxTestImports
           |
           |type StringAlias = String
           |
           |tmsFor[Option[String]]$options(Some(1) + (": 1 should be prepended (not Some(1))": StringAlias))
           |
           |""".stripMargin

      evaluateCode(testCode) shouldBe Some("1: 1 should be prepended (not Some(1))")
    }

    it should "not repeat Predef.any2stringadd behaviour for any.$plus(s: String)" in new ToolboxTest {
      val testCode: String =
        s"""$ToolboxTestImports
           |
           |tmsFor[Option[String]]$options(Some(1).$$plus(": 1 should be prepended (not Some(1))"))
           |
           |""".stripMargin

      evaluateCode(testCode) shouldBe Some("1: 1 should be prepended (not Some(1))")
    }

    it should "not repeat Predef.any2stringadd behaviour for ascripted (any: Int).$plus(s: String)" in new ToolboxTest {
      val testCode: String =
        s"""$ToolboxTestImports
           |
           |tmsFor[Option[String]]$options((Some(1): Int) + ": 1 should be prepended (not Some(1))")
           |
           |""".stripMargin

      evaluateCode(testCode) shouldBe Some("1: 1 should be prepended (not Some(1))")
    }

    it should "not repeat Predef.any2stringadd behaviour for ascripted (tts1(any): Int).$plus(s: String)" in new ToolboxTest {
      val testCode: String =
        s"""$ToolboxTestImports
           |
           |tmsFor[Option[String]]$options((tts1[Option, Int](Some(1)): Int) + ": 1 should be prepended (not Some(1))")
           |
           |""".stripMargin

      evaluateCode(testCode) shouldBe Some("1: 1 should be prepended (not Some(1))")
    }

    it should "not repeat Predef.any2stringadd behaviour for repeated any.+(s: String)" in new ToolboxTest {
      val testCode: String =
        s"""$ToolboxTestImports
           |
           |tmsFor[Option[String]]$options(Some(1) + "2" + "3" + Some(4))
           |
           |""".stripMargin

      evaluateCode(testCode) shouldBe Some("123Some(4)")
    }
  }
}
