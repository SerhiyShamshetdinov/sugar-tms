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

import TmsForTestOptions.ScalaStringVersion

import scala.concurrent.Future
import scala.util.{Success, Try}

/*
 * Created by Serhiy Shamshetdinov
 * at 18.02.2021 18:38
 */

trait TmsForTtsTests extends TmsTestBase {

  def ttsTests(tmsOptions: Seq[String]): Unit = {
    val options = literalOptions(tmsOptions)

    behavior of "tms macros general ttsN implicits with TmsOptions" + options

    it should "process first tts in built `for` with supertype of Fors Stack type and second tts with base type (Fors Stack type)" in new ToolboxTest {
      val testCode: String =
        s"""$ToolboxTestImports
           |
           |tmsFor[Option[Int]]$options {
           |  (Some(1) + Option(2)) + (Some(3) + Option(4))
           |}
           |
           |""".stripMargin

      evaluateCode(testCode) shouldBe Some(10)
    }

    it should "build correct number of fors stacks depending on tmsOptions" in new ToolboxTest {
      val testCode: String =
        s"""$ToolboxTestImports
           |
           |tmsFor[Option[Int]]$options {
           |  (Some(1) + 2) + (Some(3) + 4)
           |}
           |
           |""".stripMargin

      val (code, evaluated) = evaluatedCode(testCode)

      validateForsStacksNumber(code, tmsOptions, noArgForsStackNoParamForsStack = 1, noArgForsStackYesParamForsStack = 2, yesArgForsStackNoParamForsStack = 2, yesArgForsStackYesParamForsStack = 3)
      evaluated shouldBe Some(10)
    }

    it should "evaluate expressions in the correct order" in new ToolboxTest {
      val testCode: String =
        s"""$ToolboxTestImports
           |
           |var log: Seq[Int] = Seq()
           |
           |def logOptionInt(i: Int): Option[Int] = {
           |  log = log :+ i
           |  Some(i)
           |}
           |
           |def test(i1: Int, i2: Int)(i3: Int, i4: Int): Int = i1 * i2 + i3 * i4
           |
           |tmsFor[Option[Int]]$options(
           |  logOptionInt(1) + test(logOptionInt(2), logOptionInt(3))(logOptionInt(4), logOptionInt(5))
           |)
           |
           |log
           |""".stripMargin

      evaluateCode(testCode) shouldBe Seq(1, 2, 3, 4, 5)
    }

    it should "process expression with tts1 of each stack type" in new ToolboxTest {
      val testCode: String =
        s"""$ToolboxTestImportsWithFuture
           |
           |tmsFor[Future[List[Try[Option[Int]]]]]$options(Future.successful(1) + List(2) + Try(3) + Some(4) + 6)
           |
           |""".stripMargin

      evaluateCode(testCode).asInstanceOf[Future[_]].value.get.get shouldBe List(Try(Some(16)))
    }

    it should "process parameter with single tts to match parameter type" in new ToolboxTest {
      val testCode: String =
        s"""$ToolboxTestImports
           |
           |def tryForInt(i: Int): Try[Int] = Try(i)
           |
           |tmsFor[Option[Try[Int]]]$options(tryForInt(Some(3))) // tryForInt(tts1[Some, Int](Some(3)))
           |
           |""".stripMargin

      evaluateCode(testCode) shouldBe Some(Try(3))
    }

    it should "process parameter with tts with operation on it" in new ToolboxTest {
      val testCode: String =
        s"""$ToolboxTestImports
           |
           |def tryForInt(i: Int): Try[Int] = Try(i)
           |
           |tmsFor[Option[Try[Int]]]$options(tryForInt(Some(3) - 1)) // tryForInt(tts1[Some, Int](Some(3)) - 1)
           |
           |""".stripMargin

      evaluateCode(testCode) shouldBe Some(Try(2))
    }

    it should "process parameter with tts as parameter of expression" in new ToolboxTest {
      val testCode: String =
        s"""$ToolboxTestImports
           |
           |def tryForInt(i: Int): Try[Int] = Try(i)
           |
           |tmsFor[Option[Try[Int]]]$options(tryForInt(3 - Some(1))) // tryForInt(3 - tts1[Some, Int](Some(1)))
           |
           |""".stripMargin

      evaluateCode(testCode) shouldBe Some(Try(2))
    }

    it should "process parameter with tts with operation on it & external tts" in new ToolboxTest {
      val testCode: String =
        s"""$ToolboxTestImports
           |
           |def tryForInt(i: Int): Try[Int] = Try(i)
           |
           |tmsFor[Option[Try[Int]]]$options(tryForInt(Some(3) - 1) - 2) // tts1[Try, Int](tryForInt(tts1[Some, Int](Some(3)) - 1)) - 2
           |
           |""".stripMargin

      evaluateCode(testCode) shouldBe Some(Try(0))
    }

    it should "process expression having types turned inside out with manual tts" in new ToolboxTest {
      val testCode: String =
        s"""$ToolboxTestImports
           |
           |tmsFor[Option[Try[Int]]]$options(tts1[Try, Int](Try(Some(3) - 1)))
           |
           |""".stripMargin

      evaluateCode(testCode) shouldBe Some(Try(2))
    }

    it should "process combined expression having types turned inside out" in new ToolboxTest {
      val testCode: String =
        s"""$ToolboxTestImports
           |
           |tmsFor[Option[Try[Int]]]$options(Try(Some(3) - 1) - Some(2) - 4)
           |
           |""".stripMargin

      evaluateCode(testCode) shouldBe Some(Try(-4))
    }

    it should "process tts1 with operation inside tts1 as source" in new ToolboxTest {
      val testCode: String =
        s"""$ToolboxTestImportsWithFuture
           |
           |tmsFor[Future[Option[Int]]]$options(Future.successful(Some(2)).orElse(None) - 2) // a bit cut: tts1[Option, Int](tts1[Future, Some[Int]](Future.successful[Some[Int]](Some[Int](2))).orElse(scala.None)).-(2)
           |
           |""".stripMargin

      evaluateCode(testCode).asInstanceOf[Future[_]].value.get.get shouldBe Some(0)
    }

    it should "process tts1 with operation inside tts1 as parameter" in new ToolboxTest {
      val testCode: String =
        s"""$ToolboxTestImportsWithFuture
           |
           |tmsFor[Future[Option[Int]]]$options(1 - Future.successful(Some(2)).orElse(None))
           |
           |""".stripMargin

      evaluateCode(testCode).asInstanceOf[Future[_]].value.get.get shouldBe Some(-1)
    }

    it should "process tts1 with operation inside tts1 as source and parameter" in new ToolboxTest {
      val testCode: String =
        s"""$ToolboxTestImportsWithFuture
           |
           |tmsFor[Future[Option[Int]]]$options(
           |  Future.successful(Some(3)).orElse(None) - Future.successful(Some(2)).orElse(None)
           |)
           |
           |""".stripMargin

      evaluateCode(testCode).asInstanceOf[Future[_]].value.get.get shouldBe Some(1)
    }

    it should "process TmsApply with nested ascriptions argument : Type1 : Type2" in new ToolboxTest {
      val testCode: String =
        s"""$ToolboxTestImports
           |
           |def twiceInt(i: Int): Int = 2 * i
           |
           |tmsFor[Option[Long]]$options(1 + ((twiceInt(Some(1)): Int) : Long))
           |
           |""".stripMargin

      evaluateCode(testCode) shouldBe Some(3)
    }

    it should "process TmsApply with nested ascriptions with tts argument tts : Type1 : Type2" in new ToolboxTest {
      val testCode: String =
        s"""$ToolboxTestImports
           |
           |def twiceInt(i: Int): Int = 2 * i
           |
           |tmsFor[Option[Long]]$options(1 + (twiceInt(Some(1) : Int) : Long))
           |
           |""".stripMargin

      evaluateCode(testCode) shouldBe Some(3)
    }

    it should "process TmsApply with nested ascriptions with tts argument with operation (tts(...) : Type1 + ...) : Type2" in new ToolboxTest {
      val testCode: String =
        s"""$ToolboxTestImports
           |
           |def twiceInt(i: Int): Int = 2 * i
           |
           |tmsFor[Option[Long]]$options(1 + (twiceInt((Some(1) : Int) + 1) : Long))
           |
           |""".stripMargin

      evaluateCode(testCode) shouldBe Some(5)
    }

    it should "process TmsApply with ascripted source : Type" in new ToolboxTest {
      val testCode: String =
        s"""$ToolboxTestImports
           |
           |def twiceInt(i: Int): Int = 2 * i
           |
           |tmsFor[Option[Long]]$options((twiceInt(Some(1)): Long) + 1)
           |
           |""".stripMargin

      evaluateCode(testCode) shouldBe Some(3)
    }

    it should "process TmsApply with nested ascriptions source : Type1 : Type2" in new ToolboxTest {
      val testCode: String =
        s"""$ToolboxTestImports
           |
           |def twiceInt(i: Int): Int = 2 * i
           |
           |tmsFor[Option[Long]]$options(((twiceInt(Some(1)): Int) : Long) + 1)
           |
           |""".stripMargin

      evaluateCode(testCode) shouldBe Some(3)
    }

    it should "process TmsApply with nested ascriptions with tts source tts : Type1 : Type2" in new ToolboxTest {
      val testCode: String =
        s"""$ToolboxTestImports
           |
           |def twiceInt(i: Int): Int = 2 * i
           |
           |tmsFor[Option[Long]]$options((twiceInt(Some(1) : Int) : Long) + 1)
           |
           |""".stripMargin

      evaluateCode(testCode) shouldBe Some(3)
    }

    it should "process TmsApply with nested ascriptions with tts source with operation (tts(...) : Type1 + ...) : Type2" in new ToolboxTest {
      val testCode: String =
        s"""$ToolboxTestImports
           |
           |def twiceInt(i: Int): Int = 2 * i
           |
           |tmsFor[Option[Long]]$options((twiceInt((Some(1) : Int) + 1) : Long) + 1)
           |
           |""".stripMargin

      evaluateCode(testCode) shouldBe Some(5)
    }

    it should "process TmsApply with ascripted argument : Type" in new ToolboxTest {
      val testCode: String =
        s"""$ToolboxTestImports
           |
           |def twiceInt(i: Int): Int = 2 * i
           |
           |tmsFor[Option[Long]]$options(1 + (twiceInt(Some(1)): Long))
           |
           |""".stripMargin

      evaluateCode(testCode) shouldBe Some(3)
    }

    it should "process TmsApply with ascripted annotated argument : Type @unchecked" in new ToolboxTest {
      val testCode: String =
        s"""$ToolboxTestImports
           |
           |def twiceInt(i: Int): Int = 2 * i
           |
           |tmsFor[Option[Long]]$options(1 + (twiceInt(Some(1)): Long @unchecked))
           |
           |""".stripMargin

      evaluateCode(testCode) shouldBe Some(3)
    }

    it should "process TmsApply with arguments passed with ascription : _*" in new ToolboxTest {
      val testCode: String =
        s"""$ToolboxTestImports
           |
           |def seqInt(si: Int *): Seq[Int] = si
           |
           |tmsFor[Option[Seq[Int]]]$options(1 + seqInt( Some(Seq(1, 2, 3)): _*) )
           |
           |""".stripMargin

      evaluateCode(testCode) shouldBe Some(Seq(2, 3, 4))
    }

    it should "postprocess and flatMap yield body of the root Fors Stack with the same monadic type as the most inner used for type" in new ToolboxTest {
      val testCode: String =
        s"""$ToolboxTestImports
           |
           |tmsFor[Option[Int]]$options( Some(Some(1) + 2) )
           |
           |""".stripMargin

      val (code, evaluated) = evaluatedCode(testCode)

      code should include("flatMappedValueOfSome$")
      evaluated shouldBe Some(3)
    }

    it should "postprocess and do not flatMap yield body of the root Fors Stack with the same monadic type when the same most inner type of Stack is not used" in new ToolboxTest {
      val testCode: String =
        s"""$ToolboxTestImports
           |
           |tmsFor[Option[Option[Int]]]$options( Some(Some(1) + 2) )
           |
           |""".stripMargin

      val (code, evaluated) = evaluatedCode(testCode)

      code should not include("flatMappedValueOfSome$")
      evaluated shouldBe Some(Some(3))
    }

    it should "postprocess and do not flatMap yield body of the inner 'for' for apply parameter (actual in 'FSFAP' mode)" in new ToolboxTest {
      val testCode: String =
        s"""$ToolboxTestImports
           |
           |tmsFor[Option[Option[Int]]]$options( identity[Option[Int]]( Some(Some(1) + 2) ) )
           |
           |""".stripMargin

      val (code, evaluated) = evaluatedCode(testCode)

      code should not include("flatMappedValueOfOption$")
      evaluated shouldBe Some(Some(3))
    }

    it should "postprocess and do not flatMap yield body the of inner 'for' for apply source (actual in 'FSFAS' mode)" in new ToolboxTest {
      val testCode: String =
        s"""$ToolboxTestImports
           |
           |tmsFor[Option[Int]]$options( Some(Some(1) + 2).get )
           |
           |""".stripMargin

      val (code, evaluated) = evaluatedCode(testCode)

      code should not include("flatMappedValueOfOption$")
      evaluated shouldBe Some(3)
    }

    it should "process tts2 + T" in new ToolboxTest {
      val testCode: String =
        s"""$ToolboxTestImports
           |
           |tmsFor[Try[Option[Int]]]$options(Try(Some(1)) + 2)
           |
           |""".stripMargin

      evaluateCode(testCode) shouldBe Try(Some(3))
    }

    it should "process tts3 + T" in new ToolboxTest {
      val testCode: String =
        s"""$ToolboxTestImportsWithFuture
           |
           |tmsFor[Future[Try[Option[Int]]]]$options(Future.successful(Try(Some(1))) + 2)
           |
           |""".stripMargin

      evaluateCode(testCode).asInstanceOf[Future[_]].value.get.get shouldBe Try(Some(3))
    }

    it should "process tts4 + T" in new ToolboxTest {
      val testCode: String =
        s"""$ToolboxTestImportsWithFuture
           |
           |tmsFor[Future[List[Try[Option[Int]]]]]$options(Future.successful(List(Try(Some(1)))) + 2)
           |
           |""".stripMargin

      evaluateCode(testCode).asInstanceOf[Future[_]].value.get.get shouldBe List(Try(Some(3)))
    }

    it should "process tts5 + T" in new ToolboxTest {
      val testCode: String =
        s"""$ToolboxTestImportsWithFuture
           |
           |tmsFor[Future[List[Try[Option[Seq[Int]]]]]]$options(Future.successful(List(Try(Some(Seq(1))))) + 2)
           |
           |""".stripMargin

      evaluateCode(testCode).asInstanceOf[Future[_]].value.get.get shouldBe List(Try(Some(Seq(3))))
    }

    it should "process tts5 + tts4 + tts3 + tts2 + tts1 + T with inner Stacks" in new ToolboxTest {
      val testCode: String =
        s"""$ToolboxTestImportsWithFuture
           |
           |tmsFor[Future[List[Try[Option[Seq[Int]]]]]]$options(
           |  Future.successful(List(Try(Some(Seq(1))))) + List(Try(Some(Seq(2)))) + Try(Some(Seq(3))) + Some(Seq(4)) + Seq(5) + 6
           |)
           |
           |""".stripMargin

      evaluateCode(testCode).asInstanceOf[Future[_]].value.get.get shouldBe List(Try(Some(Seq(21))))
    }

    it should "process tail-grouped tts3 + (tts2 + (tts1 + T))" in new ToolboxTest {
      val testCode: String =
        s"""$ToolboxTestImportsWithFuture
           |
           |tmsFor[Future[Try[Option[Int]]]]$options(Future.successful(Try(Some(3))) - (Try(Some(2)) - (Some(1) - 4)))
           |
           |""".stripMargin

      evaluateCode(testCode).asInstanceOf[Future[_]].value.get.get shouldBe Try(Some(-2))
    }

    it should "process tts3 + tts2 with skipped type + T" in new ToolboxTest {
      val testCode: String =
        s"""$ToolboxTestImportsWithFuture
           |
           |tmsFor[Future[Try[Option[Int]]]]$options(Future.successful(Try(Some(3))) - Future.successful(Some(2)) - 4)
           |
           |""".stripMargin

      evaluateCode(testCode).asInstanceOf[Future[_]].value.get.get shouldBe Try(Some(-3))
    }

    it should "process expression with tts2 of different paired stack types" in new ToolboxTest {
      val testCode: String =
        s"""$ToolboxTestImportsWithFuture
           |
           |tmsFor[Future[Try[Option[Int]]]]$options(Future.successful(Try(3)) * Try(Some(2)) - Future.successful(Some(1)) - 4)
           |
           |""".stripMargin

      evaluateCode(testCode).asInstanceOf[Future[_]].value.get.get shouldBe Try(Some(1))
    }

    it should "flatten nested ttsN like tts2(...) = tts1(tts1(...)) in source expression: it never happens with auto inserting by implicits but may be coded manually" in new ToolboxTest {
      val testCode: String =
        s"""$ToolboxTestImports
           |
           |tmsFor[Try[Option[Int]]]$options(tts1[Some, Int](tts1[Try, Some[Int]](Try(Some(3)))) - 1)
           |
           |""".stripMargin

      evaluateCode(testCode) shouldBe Try(Some(2))
    }

    it should "flatten nested ttsN with ascriptions like tts2(...): Int = tts1(tts1(...): Some[Int]): Int in source expression" in new ToolboxTest {
      val testCode: String =
        s"""$ToolboxTestImports
           |
           |tmsFor[Try[Option[Int]]]$options((tts1[Some, Int](tts1[Try, Some[Int]](Try(Some(3))): Some[Int]): Int) - 1)
           |
           |""".stripMargin

      evaluateCode(testCode) shouldBe Try(Some(2))
    }

    it should "flatten nested ttsN like tts2(...) = tts1(tts1(...)) as parameter: it never happens with auto inserting by implicits but may be coded manually" in new ToolboxTest {
      val testCode: String =
        s"""$ToolboxTestImports
           |
           |tmsFor[Try[Option[Int]]]$options(1 - tts1[Some, Int](tts1[Try, Some[Int]](Try(Some(3)))))
           |
           |""".stripMargin

      evaluateCode(testCode) shouldBe Try(Some(-2))
    }

    it should "flatten nested ttsN with ascriptions like tts2(...): Int = tts1(tts1(...): Some[Int]): Int as parameter" in new ToolboxTest {
      val testCode: String =
        s"""$ToolboxTestImports
           |
           |tmsFor[Try[Option[Int]]]$options(1 - tts1[Some, Int](tts1[Try, Some[Int]](Try(Some(3))): Some[Int]): Int)
           |
           |""".stripMargin

      evaluateCode(testCode) shouldBe Try(Some(-2))
    }

    it should "postprocess and inline single pre evaluations of all types of root & inner Fors Stacks when its enum is first" in new ToolboxTest {
      val testCode: String =
        s"""$ToolboxTestImports
           |
           |tmsFor[Try[Option[Int]]]$options {
           |  Some(1) + Try(2)
           |}
           |
           |""".stripMargin

      val (code, evaluated) = evaluatedCode(testCode)

      countVals(code) shouldBe 0
      evaluated shouldBe Success(Some(3))
    }

  }

  def ttsCollectionsTests(tmsOptions: Seq[String]): Unit = {
    val options = literalOptions(tmsOptions)

    behavior of "tms macros collections ttsN implicits with TmsOptions" + options

    it should "process first tts on collection in built `for` with supertype of Fors Stack type and second tts with base type (Fors Stack type)" in new ToolboxTest {
      val testCode: String =
        s"""$ToolboxTestImports
           |
           |tmsFor[Seq[Int]]$options {
           |  (List(1) + Seq(2)) + (List(3) + Seq(4))
           |}
           |
           |""".stripMargin

      evaluateCode(testCode) shouldBe Seq(10)
    }

    it should "access .min of inner string, not .min(char) of RichChar" in new ToolboxTest {
      val testCode: String =
        s"""$ToolboxTestImports
           |
           |tmsFor[Option[Char]]$options(Some("123").min)
           |
           |""".stripMargin

      evaluateCode(testCode) shouldBe Some('1')
    }

    it should "access .length of Seq" in new ToolboxTest {
      val testCode: String =
        s"""$ToolboxTestImports
           |
           |tmsFor[Option[Int]]$options(Some(Seq(1, 2)).length)
           |
           |""".stripMargin

      evaluateCode(testCode) shouldBe Some(2)
    }

    it should "access .length of inner strings, not List" in new ToolboxTest {
      val testCode: String =
        s"""$ToolboxTestImports
           |
           |tmsFor[Option[List[Int]]]$options(Some(List("1", "12")).length)
           |
           |""".stripMargin

      evaluateCode(testCode) shouldBe Some(List(1, 2))
    }

    it should "access .length of inner List with explicit tts, not strings" in new ToolboxTest {
      val testCode: String =
        s"""$ToolboxTestImports
           |
           |tmsFor[Option[Int]]$options(tts1(Some(List("1", "12"))).length)
           |
           |""".stripMargin

      evaluateCode(testCode) shouldBe Some(2)
    }

    it should "access .min of inner Int with comparing value via RichInt, not .min of Seq" in new ToolboxTest {
      val testCode: String =
        s"""$ToolboxTestImports
           |
           |tmsFor[Option[Seq[Int]]]$options(Some(Seq(1, 12)).min(6))
           |
           |""".stripMargin

      evaluateCode(testCode) shouldBe Some(Seq(1, 6))
    }

    it should "access .min of inner Seq with explicit tts1, not .min of Int via RichInt" in new ToolboxTest {
      val testCode: String =
        s"""$ToolboxTestImports
           |
           |tmsFor[Option[Int]]$options(tts1(Some(Seq(1, 12))).min)
           |
           |""".stripMargin

      evaluateCode(testCode) shouldBe Some(1)
    }

    it should "access .length of inner strings, not Array" in new ToolboxTest {
      val testCode: String =
        s"""$ToolboxTestImports
           |
           |tmsFor[Option[Array[Int]]]$options(Some(Array("1", "12")).length)
           |
           |""".stripMargin

      evaluateCode(testCode).asInstanceOf[Option[Array[Int]]].map(_.mkString) shouldBe Some(Array(1, 2)).map(_.mkString)
    }

    it should "access .length of inner Array with explicit tts, not strings" in new ToolboxTest {
      val testCode: String =
        s"""$ToolboxTestImports
           |
           |tmsFor[Option[Int]]$options(tts1(Some(Array("1", "12"))).length)
           |
           |""".stripMargin

      evaluateCode(testCode) shouldBe Some(2)
    }

    it should "access .min of inner Int with comparing value via RichInt, not .min of Array" in new ToolboxTest {
      val testCode: String =
        s"""$ToolboxTestImports
           |
           |tmsFor[Option[Array[Int]]]$options(Some(Array(1, 12)).min(6))
           |
           |""".stripMargin

      evaluateCode(testCode).asInstanceOf[Option[Array[Int]]].map(_.mkString) shouldBe Some(Array(1, 6)).map(_.mkString)
    }

    it should "access .min of inner Array with explicit tts1, not .min of Int via RichInt" in new ToolboxTest {
      val testCode: String =
        s"""$ToolboxTestImports
           |
           |tmsFor[Option[Int]]$options(tts1(Some(Array(1, 12))).min)
           |
           |""".stripMargin

      evaluateCode(testCode) shouldBe Some(1)
    }

    // This is not possible due to Predef (and so: tts) implicits for Array exist. This is a scala implementation restriction:
    // tts Array implicits always have higher priority than tss Seq (implicits than any! collection type tts)
//    it should "access .length of inner Seq, not .length of Array" in new MacroTest {
//      val testCode: String =
//        s"""$importsAndVals
//           |
//           |tmsFor[Option[Array[Int]]]$options(Some(Array(Seq(1, 12))).length)
//           |
//           |""".stripMargin
//
//      evaluateCode(testCode).asInstanceOf[Option[Array[Int]]].map(_.mkString) shouldBe Some(Array(2)).map(_.mkString)
//    }

    it should "access .length of Array with explicit tts1, not .length of inner Seq" in new ToolboxTest {
      val testCode: String =
        s"""$ToolboxTestImports
           |
           |tmsFor[Option[Int]]$options(tts1(Some(Array(Seq(1, 12)))).length)
           |
           |""".stripMargin

      evaluateCode(testCode) shouldBe Some(1)
    }

    it should "access .length of inner Array, not .length of Seq" in new ToolboxTest {
      val testCode: String =
        s"""$ToolboxTestImports
           |
           |tmsFor[Option[Seq[Int]]]$options(Some(Seq(Array(1, 12))).length)
           |
           |""".stripMargin

      evaluateCode(testCode) shouldBe Some(Seq(2))
    }

    it should "access .length of Seq with explicit tts1, not .length of inner Array" in new ToolboxTest {
      val testCode: String =
        s"""$ToolboxTestImports
           |
           |tmsFor[Option[Int]]$options(tts1(Some(Seq(Array(1, 12)))).length)
           |
           |""".stripMargin

      evaluateCode(testCode) shouldBe Some(1)
    }

    it should "access .min of inner Int with comparing value via RichInt, not .min of Seq inside Array" in new ToolboxTest {
      val testCode: String =
        s"""$ToolboxTestImports
           |
           |tmsFor[Option[Array[Seq[Int]]]]$options(Some(Array(Seq(1, 12))).min(6))
           |
           |""".stripMargin

      evaluateCode(testCode).asInstanceOf[Option[Array[Seq[Int]]]].map(_.mkString) shouldBe Some(Array(Seq(1, 6))).map(_.mkString)
    }

    it should "access .min of inner Seq inside Array with explicit tts2, not .min of Int via RichInt" in new ToolboxTest {
      val testCode: String =
        s"""$ToolboxTestImports
           |
           |tmsFor[Option[Array[Int]]]$options(tts2(Some(Array(Seq(1, 12)))).min)
           |
           |""".stripMargin

      evaluateCode(testCode).asInstanceOf[Option[Array[Int]]].map(_.mkString) shouldBe Some(Array(1)).map(_.mkString)
    }

    it should "access .min of inner Int with comparing value via RichInt, not .min of Array inside Seq" in new ToolboxTest {
      val testCode: String =
        s"""$ToolboxTestImports
           |
           |tmsFor[Option[Seq[Array[Int]]]]$options(Some(Seq(Array(1, 12))).min(6))
           |
           |""".stripMargin

      evaluateCode(testCode).asInstanceOf[Option[Seq[Array[Int]]]].map(_.map(_.mkString)) shouldBe Some(Seq(Array(1, 6))).map(_.map(_.mkString))
    }

    it should "access .min of inner Array inside Seq with explicit tts2, not .min of Int via RichInt" in new ToolboxTest {
      val testCode: String =
        s"""$ToolboxTestImports
           |
           |tmsFor[Option[Seq[Int]]]$options(tts2(Some(Seq(Array(1, 12)))).min)
           |
           |""".stripMargin

      evaluateCode(testCode) shouldBe Some(Seq(1))
    }

    it should "calculate Seq[Int] + Seq[Int]" in new ToolboxTest {
      val testCode: String =
        s"""$ToolboxTestImports
           |
           |tmsFor[Seq[Int]]$options {
           |  Seq(1, 2) + Seq(3, 4)
           |}
           |
           |""".stripMargin

      evaluateCode(testCode) shouldBe Seq(1 + 3, 1 + 4, 2 + 3, 2 + 4)
    }

    it should "operate on tts1 Seq" in new ToolboxTest {
      val testCode: String =
        s"""$ToolboxTestImports
           |
           |tmsFor[Seq[Int]]$options {
           |  Seq(1, 2) * 3
           |}
           |
           |""".stripMargin

      evaluateCode(testCode) shouldBe Seq(3, 6)
    }

    it should "operate on tts2 Seq flatMapping it" in new ToolboxTest {
      val testCode: String =
        s"""$ToolboxTestImports
           |
           |tmsFor[Seq[Int]]$options {
           |  Seq(Seq(1, 2)) * 3
           |}
           |
           |""".stripMargin

      evaluateCode(testCode) shouldBe Seq(3, 6)
    }

    it should "calculate Array[Int] + Array[Int]" in new ToolboxTest {
      val testCode: String =
        s"""$ToolboxTestImports
           |
           |tmsFor[Array[Int]]$options {
           |  Array(1, 2) + Array(3, 4)
           |}
           |
           |""".stripMargin

      evaluateCode(testCode) shouldEqual Array(1 + 3, 1 + 4, 2 + 3, 2 + 4)
    }

    it should "operate on tts1 Array" in new ToolboxTest {
      val testCode: String =
        s"""$ToolboxTestImports
           |
           |tmsFor[Array[Int]]$options {
           |  Array(1, 2) * 3
           |}
           |
           |""".stripMargin

      evaluateCode(testCode) shouldEqual Array(3, 6)
    }

    if (SemanticVersion(ScalaStringVersion) < "2.12.13") { // see comment in "else"

      it should "operate on tts2 Array flatMapping it" in new ToolboxTest {
        val testCode: String =
          s"""$ToolboxTestImports
             |
             |tmsFor[Array[Int]]$options {
             |  Array(Array(1, 2)) * 3
             |}
             |
             |""".stripMargin

        evaluateCode(testCode) shouldEqual Array(3, 6)
      }

      it should "operate on tts3 Array flatMapping it" in new ToolboxTest {
        val testCode: String =
          s"""$ToolboxTestImports
             |
             |tmsFor[Array[Int]]$options {
             |  Array(Array(Array(1, 2))) * 3
             |}
             |
             |""".stripMargin

        evaluateCode(testCode) shouldEqual Array(3, 6)
      }

      it should "operate on tts4 Array flatMapping it" in new ToolboxTest {
        val testCode: String =
          s"""$ToolboxTestImports
             |
             |tmsFor[Array[Int]]$options {
             |  Array(Array(Array(Array(1, 2)))) * 3
             |}
             |
             |""".stripMargin

        evaluateCode(testCode) shouldEqual Array(3, 6)
      }

      it should "operate on tts5 Array flatMapping it" in new ToolboxTest {
        val testCode: String =
          s"""$ToolboxTestImports
             |
             |tmsFor[Array[Int]]$options {
             |  Array(Array(Array(Array(Array(1, 2))))) * 3
             |}
             |
             |""".stripMargin

        evaluateCode(testCode) shouldEqual Array(3, 6)
      }

      it should "append Array to each one inside another Array with explicit tts1" in new ToolboxTest {
        val testCode: String =
          s"""$ToolboxTestImports
             |
             |tmsFor[Array[Array[Int]]]$options {
             |  tts1(Array(Array(0), Array(1, 2))) ++[Int, Array[Int]] Array(3)
             |}
             |
             |""".stripMargin

        evaluateCode(testCode) shouldEqual Array(Array(0, 3), Array(1, 2, 3))
      }

      it should "append Array to each one inside another Array and then flatMap result" in new ToolboxTest {
        val testCode: String =
          s"""$ToolboxTestImports
             |
             |tmsFor[Array[Int]]$options {
             |  tts1(Array(Array(0), Array(1, 2))) ++[Int, Array[Int]] Array(3)
             |}
             |
             |""".stripMargin

        evaluateCode(testCode) shouldEqual Array(0, 3, 1, 2, 3)
      }
    } else { // starting 2.12.13 & in 2.13 toolbox fails with strange error: reflective compilation has failed:
      //Error while emitting <no source file>
      //  assertion failed:
      //  expected class type in classOf[T], found primitive type I

      it should "operate on tts2 Array flatMapping it: NON-TOOLBOX" in {
        {
          import sands.sugar.tms.TransparentMonads._

          tmsFor[Array[Int]]() {
            Array(Array(1, 2)) * 3
          }
        } shouldEqual Array(3, 6)
      }

      it should "operate on tts3 Array flatMapping it: NON-TOOLBOX" in {
        {
          import sands.sugar.tms.TransparentMonads._

          tmsFor[Array[Int]]() {
            Array(Array(Array(1, 2))) * 3
          }
        } shouldEqual Array(3, 6)
      }

      it should "operate on tts4 Array flatMapping it: NON-TOOLBOX" in {
        {
          import sands.sugar.tms.TransparentMonads._

          tmsFor[Array[Int]]() {
            Array(Array(Array(Array(1, 2)))) * 3
          }
        } shouldEqual Array(3, 6)
      }

      it should "operate on tts5 Array flatMapping it: NON-TOOLBOX" in {
        {
          import sands.sugar.tms.TransparentMonads._

          tmsFor[Array[Int]]() {
            Array(Array(Array(Array(Array(1, 2))))) * 3
          }
        } shouldEqual Array(3, 6)
      }

      it should "append lem to each Array inside another Array with explicit tts1: NON-TOOLBOX" in {
        {
          import sands.sugar.tms.TransparentMonads._

          tmsFor[Array[Array[Int]]]() {
            tts1(Array(Array(0), Array(1, 2))) :+ 3
          }
        } shouldEqual Array(Array(0, 3), Array(1, 2, 3))
      }

      it should "append lem to each Array inside another Array and then flatMap result: NON-TOOLBOX" in {
        {
          import sands.sugar.tms.TransparentMonads._

          tmsFor[Array[Int]]() {
            tts1(Array(Array(0), Array(1, 2))) :+ 3
          }
        } shouldEqual Array(0, 3, 1, 2, 3)
      }
    }

    //NON-TOOLBOX does not helps: strange bug in 2.13+ only (< 2.13 works ok) if to pass `identity[Int](_)` anon function to .compose it fails with
    //reflective typecheck has failed: not found: value x$1
    // compiler generates PartialFunction argument instead of Function 1
    //while generating strange output code (really no x$1 is defined, but <empty>!!!):
    //{
    //  for {
    //    valueOfTry$macro$1 <- scala.util.Try.apply[Array[Int]](scala.Array.apply(1, 2))
    //  } yield {
    //    valueOfTry$macro$1.compose[scala.Int](<empty> match {
    //      case (defaultCase$ @ _) => scala.Predef.identity[scala.Int](x$1)
    //    }).apply(0)
    //  }
    //}
    //looks like the reason is .compose accepts 2 variants argument: partial function & function. Compiler tries to lift anon function to part.function & fails?
    //test fix is to pass function .compose[Int](identity[Int] _) - not a anon function
//    it should "accept Array .compose with anonymous Function (not a PartialFunction & not a function value)" in new MacroTest {
//      val testCode: String =
//        s"""$importsAndVals
//           |
//           |tmsFor[Try[Int]]$options {
//           |  Try(Array(1, 2)).compose[Int](identity[Int](_)).apply(0)
//           |}
//           |
//           |""".stripMargin
//
//      evaluateCode(testCode) shouldBe Success(1)
//    }

    it should "accept Array .compose with Function" in new ToolboxTest {
      val testCode: String =
        s"""$ToolboxTestImports
           |
           |tmsFor[Try[Int]]$options {
           |  Try(Array(1, 2)).compose[Int](identity[Int] _).apply(0)
           |}
           |
           |""".stripMargin

      evaluateCode(testCode) shouldBe Success(1)
    }

    it should "process tts with Seq of primitive type passed where wider type parameter is expected" in new ToolboxTest {
      val testCode: String =
        s"""$ToolboxTestImports
           |
           |tmsFor[Option[Seq[Long]]]$options(identity[Long](Some(Seq(1, 2))))
           |
           |""".stripMargin

      evaluateCode(testCode) shouldBe Some(Seq(1L, 2L))
    }

    it should "process tts with Seq of objects & its primitive type accessor passed where wider type parameter is expected" in new ToolboxTest {
      val testCode: String =
        s"""$ToolboxTestImports
           |
           |class TestClassWithInt(val i: Int)
           |
           |tmsFor[Option[Seq[Long]]]$options(
           |  identity[Long](
           |    Some( Seq(new TestClassWithInt(3), new TestClassWithInt(4)) ).i
           |  )
           |)
           |
           |""".stripMargin

      evaluateCode(testCode) shouldBe Some(Seq(3L, 4L))
    }

    it should "process tts with Array of primitive type passed where wider type parameter is expected" in new ToolboxTest {
      val testCode: String =
        s"""$ToolboxTestImports
           |
           |tmsFor[Option[Array[Long]]]$options(identity[Long](Some(Array(1, 2))))
           |
           |""".stripMargin

      Array(1L, 2L) shouldEqual evaluateCode(testCode).asInstanceOf[Option[Array[Long]]].get
    }

    //2.13 <toolbox>:16: error: type mismatch;
    // found   : Any
    // required: scala.reflect.ClassTag[TestClassWithInt]
    //    Some( Array(new TestClassWithInt(3), new TestClassWithInt(4)) ).i
    it should "process tts with Array of objects & its primitive type accessor passed where wider type parameter is expected: NON-TOOLBOX" in {
      {
        import sands.sugar.tms.TransparentMonads._

        class TestClassWithInt(val i: Int)

        tmsFor[Option[Array[Long]]]()(
          identity[Long](
            Some( Array(new TestClassWithInt(3), new TestClassWithInt(4)) ).i
          )
        )
      }.get shouldEqual Array(3L, 4L)
    }
  }
}
