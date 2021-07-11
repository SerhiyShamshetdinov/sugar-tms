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

import scala.tools.reflect.ToolBoxError
import scala.util.{Failure, Success, Try}

/*
 * Created by Serhiy Shamshetdinov
 * at 11.12.2020 11:52
 */

class TmsForGeneralTest extends TmsTestBase
  with TmsForTtsTests
  with TmsForTypeClassesTests {

  generalBehaviourTests(SimpleForsStackOptions)

  testWithForsStackWithPreEvaluationVariants(tmsOptions => {
    ttsTests(tmsOptions)
    ttsCollectionsTests(tmsOptions)
    typeClassesTests(tmsOptions)
  })

  // should be run with SimpleForsStackOptions only
  def generalBehaviourTests(tmsOptions: Seq[String]): Unit = {
    val options = literalOptions(tmsOptions)

    behavior of "tms macros General Behaviour with TmsOptions" + options

    it should "fail when implicit ttsNx conversion is applied outside tms macros" in new ToolboxTest {
      val testCode: String =
        s"""$ToolboxTestImports
           |
           |Some(1): Int // TOOLBOX reasonably errors: Reference to method tts1intIdentity in trait TmsImplicits should not have survived
           |
           |""".stripMargin

      Try(evaluateCode(testCode)) shouldBe
        Failure(ToolBoxError(s"reflective compilation has failed:${System.lineSeparator}${System.lineSeparator}Reference to method tts1intIdentity in trait TmsImplicits should not have survived past type checking,\nit should have been processed and eliminated during expansion of an enclosing macro."))
    }

    it should "fail when tms types stack has no monadic type on top of the stack" in new ToolboxTest {
      val testCode: String =
        s"""$ToolboxTestImports
           |
           |def identityInt(i: Int): Int = i
           |
           |tmsFor[Int]$options(identityInt(Some(1)))
           |
           |""".stripMargin

      Try(evaluateCode(testCode)) shouldBe
        Failure(ToolBoxError("reflective typecheck has failed: top of the tms types stack should contain at least one type that (possibly after dealiasing) has exactly one type parameter"))
    }

    it should "fail since not possible to reconstruct expression of requested tms types stack" in new ToolboxTest {
      val testCode: String =
        s"""$ToolboxTestImports
           |
           |tmsFor[Try[Option[List[Int]]]]$options( Some(Try(1)) - 2 ) // tts2[Some, Try, Int](Some(Try(1))) - 2 : while opening Some(Try(1)) Some should be outer type then Try type, so macro type should be Option[Try[_]] or has such stack fragment
           |
           |""".stripMargin

      Try(evaluateCode(testCode)) should matchPattern {
        case Failure(ToolBoxError(msg, _)) if msg.startsWith(
          "reflective typecheck has failed: type scala.util.Try could not be found in the non-reserved rest of the types stack [List]. Reserved types (from outer to the last used inner one inclusive) are [Try, Option]") =>
      }
    }

    it should "warn no tts() found" in new ToolboxTest {
      val testCode: String =
        s"""$ToolboxTestImports
           |
           |tmsFor[Option[Int]]$options(Some(1)) // reasonably warns: no tts operations (reachable by TmsTree AST) are found
           |
           |""".stripMargin

      evaluateCode(testCode) shouldBe Some(1)
    }

    it should "fail when outer stack type manipulations are absent" in new ToolboxTest {
      val testCode: String =
        s"""$ToolboxTestImports
           |
           |tmsFor[Try[Option[Int]]]$options(Some(1) + 2)
           |
           |""".stripMargin

      Try(evaluateCode(testCode)) shouldBe
        Failure(ToolBoxError("reflective typecheck has failed: type mismatch;\n found   : Option[Int]\n required: scala.util.Try[Option[Int]]"))
    }

    it should "fail when one stack type manipulations are absent (empty forCollector for the type specified in the Stack)" in new ToolboxTest {
      val testCode: String =
        s"""$ToolboxTestImports
           |
           |tmsFor[Try[Option[Int]]]$options(scala.util.Success(1) + 2)
           |
           |""".stripMargin

      Try(evaluateCode(testCode)) shouldBe
        Failure(ToolBoxError("reflective typecheck has failed: type mismatch;\n found   : Int\n required: Option[Int]"))
    }

    it should "accept val as a single statement resulting Unit inner value" in new ToolboxTest {
      val testCode: String =
        s"""$ToolboxTestImports
           |
           |tmsFor[Option[Unit]]$options {
           |  val i: Int = Some(1)
           |}
           |
           |""".stripMargin

      evaluateCode(testCode) shouldBe Some({})
    }

    it should "bypass tts1 for String + Any" in new ToolboxTest {
      val testCode: String =
        s"""$ToolboxTestImports
           |
           |"String plus Some(1) should concatenate Some(1) (not 1): " + Some(1)
           |
           |""".stripMargin

      evaluateCode(testCode) shouldBe "String plus Some(1) should concatenate Some(1) (not 1): Some(1)"
    }

    it should "accept short import when TransparentMonads is imported separately" in new ToolboxTest {
      val testCode: String =
        s"""import sands.sugar.tms.TransparentMonads
           |{
           |  import TransparentMonads._
           |
           |  tmsFor[Option[Int]]$options(Some(1) + 2)
           |}
           |""".stripMargin

      evaluateCode(testCode) shouldBe Some(3)
    }

    it should "process tms call inside the code passed to outer tms call" in new ToolboxTest {
      val testCode: String =
        s"""$ToolboxTestImports
           |
           |tmsFor[Option[Int]]$options(1 + tmsFor[Option[Int]]$options(Some(2) + 1))
           |
           |""".stripMargin

      evaluateCode(testCode) shouldBe Some(4)
    }

    it should "dealias passed stack types to one parameter types dealiasing chains: tms types stack is List(scala.util.Try, scala.Option)" in new ToolboxTest {
      val testCode: String =
        s"""$ToolboxTestImports
           |
           |class TestContainer {
           |  type Num = Int
           |  type MonadicNum = Option[Num]
           |  type MonadicVal = MonadicNum
           |  type MonadicTryFlowController = Try[MonadicVal]
           |  type MonadicFlowController = MonadicTryFlowController
           |
           |  def test = tmsFor[MonadicFlowController]$options(Try(Some(3)) + 1)
           |}
           |
           |new TestContainer().test
           |
           |""".stripMargin

      evaluateCode(testCode) shouldBe Try(Some(4))
    }

    it should "dealias passed stack types to one parameter types ignoring inner chain without 1 parameter types: tms types stack is the only List(scala.Option)" in new ToolboxTest {
      val testCode: String =
        s"""$ToolboxTestImports
           |
           |class TestContainer {
           |  type EitherStringInt = Either[String, Int]
           |  type Inner = EitherStringInt
           |  type MonadicVal = Inner
           |  type Monadic = Option[MonadicVal]
           |
           |  def test = tmsFor[Monadic]$options(Some(Left[Int, String](1)).swap)
           |}
           |
           |new TestContainer().test
           |
           |""".stripMargin

      evaluateCode(testCode) shouldBe Some(Right[String, Int](1))
    }

    it should "work with expressions containing aliased types" in new ToolboxTest {
      val testCode: String =
        s"""$ToolboxTestImports
           |
           |class TestContainer {
           |  type MonadicInt = Option[Int]
           |  type MonadicIntFlowController = Try[MonadicInt]
           |
           |  def test = tmsFor[MonadicIntFlowController]$options((Try(Some(3): MonadicInt): MonadicIntFlowController) + 1)
           |}
           |
           |new TestContainer().test
           |
           |""".stripMargin

      evaluateCode(testCode) shouldBe Try(Some(4))
    }

    if (ScalaStringVersion >= "2.12") {
      it should "not replace one parameter types to definitions in passed tms types stack and work with expression of this type" in new ToolboxTest {
        val testCode: String =
          s"""$ToolboxTestImports
             |
             |class TestContainer {
             |  type Monadic[T] = Either[Throwable, T]
             |  object Monadic {
             |    def apply[V](v: V): Monadic[V] = Right[Throwable, V](v)
             |  }
             |
             |  def test = tmsFor[Monadic[Int]]$options(Monadic[Int](3) + 1)
             |}
             |
             |new TestContainer().test
             |
             |""".stripMargin

        evaluateCode(testCode) shouldBe Right(4)
      }
    }

    it should "accept redefined one parameter type as is and work with expression of its definition type" in new ToolboxTest {
      val testCode: String =
        s"""$ToolboxTestImports
           |
           |class TestContainer {
           |  type Monadic[T] = Option[T]
           |
           |  def test: Monadic[Int] = tmsFor[Monadic[Int]]$options(Some(0) + 1)
           |}
           |
           |new TestContainer().test
           |
           |""".stripMargin

      evaluateCode(testCode) shouldBe Some(1)
    }

    it should "accept redefined one parameter type as is and work with expression of this type" in new ToolboxTest {
      val testCode: String =
        s"""$ToolboxTestImports
           |
           |class TestContainer {
           |  type Monadic[T] = Option[T]
           |
           |  def test: Monadic[Int] = tmsFor[Monadic[Int]]$options((Some(2): Monadic[Int]) + 1)
           |}
           |
           |new TestContainer().test
           |
           |""".stripMargin

      evaluateCode(testCode) shouldBe Some(3)
    }

    it should "accept definition type of types stack and work with expression of this redefined one parameter type" in new ToolboxTest {
      val testCode: String =
        s"""$ToolboxTestImports
           |
           |class TestContainer {
           |  type Monadic[T] = Option[T]
           |
           |  def test: Option[Int] = tmsFor[Option[Int]]$options((Some(3): Monadic[Int]) + 1)
           |}
           |
           |new TestContainer().test
           |
           |""".stripMargin

      evaluateCode(testCode) shouldBe Some(4)
    }

    it should "process yield of the inner monadic type not used in built Fors Stack" in new ToolboxTest {
      val testCode: String =
        s"""$ToolboxTestImports
           |
           |tmsFor[Option[Try[Int]]]$options( Try[Int](Some(1)) )
           |
           |""".stripMargin

      val (code, evaluated) = evaluatedCode(testCode)

      code should not include("flatMappedValueOfSome$")
      evaluated shouldBe Some(Try(1))
    }

    it should "postprocess and flatMap yield of the inner monadic type not used in built Fors Stack" in new ToolboxTest {
      val testCode: String =
        s"""$ToolboxTestImports
           |
           |tmsFor[Option[Try[Int]]]$options( Some(Try[Int](Some(1))) )
           |
           |""".stripMargin

      val (code, evaluated) = evaluatedCode(testCode)

      code should include("flatMappedValueOfSome$")
      evaluated shouldBe Some(Try(1))
    }

    it should "Pre Evaluate No Types" in new ToolboxTest {
      private val options = literalOptions(tmsOptions :+ "Pre Evaluate No Types")

      val testCode: String =
        s"""$ToolboxTestImports
           |
           |tmsFor[Try[Option[Int]]]$options( Some(Try(1) + 2) + 3 )
           |
           |""".stripMargin

      val (code, evaluated) = evaluatedCode(testCode)

      countVals(code) shouldBe 0
      evaluated shouldBe Success(Some(6))
    }

    it should "Pre Evaluate Try with simple name specified" in new ToolboxTest {
      private val options = literalOptions(tmsOptions :+ "Pre Evaluate Try")

      val testCode: String =
        s"""$ToolboxTestImports
           |
           |tmsFor[Try[Option[Int]]]$options {
           |  Try(0) + 0 // to prevent single pre eval optimization
           |  Some(Try(1) + 2) + 3
           |}
           |
           |""".stripMargin

      val (code, evaluated) = evaluatedCode(testCode)

      countVals(code) shouldBe 2
      evaluated shouldBe Success(Some(6))
    }

    it should "Pre Evaluate Option with FQN specified" in new ToolboxTest {
      private val options = literalOptions(tmsOptions :+ "Pre Evaluate scala.Option")

      val testCode: String =
        s"""$ToolboxTestImports
           |
           |tmsFor[Try[Option[Int]]]$options {
           |  Some(0) + 0 // to prevent single pre eval optimization
           |  Some(Try(1) + 2) + 3
           |}
           |
           |""".stripMargin

      val (code, evaluated) = evaluatedCode(testCode)

      countVals(code) shouldBe 2
      evaluated shouldBe Success(Some(6))
    }

    it should "Pre Evaluate Option with simple name & FQN specified" in new ToolboxTest {
      private val options = literalOptions(tmsOptions :+ "Pre Evaluate Option, scala.Option")

      val testCode: String =
        s"""$ToolboxTestImports
           |
           |tmsFor[Try[Option[Int]]]$options {
           |  Some(0) + 0 // to prevent single pre eval optimization
           |  Some(Try(1) + 2) + 3
           |}
           |
           |""".stripMargin

      val (code, evaluated) = evaluatedCode(testCode)

      countVals(code) shouldBe 2
      evaluated shouldBe Success(Some(6))
    }

    it should "Pre Evaluate All Types" in new ToolboxTest {
      private val options = literalOptions(tmsOptions :+ "Pre Evaluate All Types")

      val testCode: String =
        s"""$ToolboxTestImports
           |
           |tmsFor[Try[Option[Int]]]$options {
           |  Try(0) + 0 // to prevent single Try pre eval optimization
           |  Some(0) + 0 // to prevent single Option pre eval optimization
           |  Some(Try(1) + 2) + 3
           |}
           |
           |""".stripMargin

      val (code, evaluated) = evaluatedCode(testCode)

      countVals(code) shouldBe 4
      evaluated shouldBe Success(Some(6))
    }

    it should "Pre Evaluate Arrays" in new ToolboxTest {
      private val options = literalOptions(tmsOptions :+ "Pre Evaluate Array")

      val testCode: String =
        s"""$ToolboxTestImports
           |
           |tmsFor[Array[Int]]$options {
           |  Array(1, 2) + Array(3)
           |}
           |
           |""".stripMargin

      val (code, evaluated) = evaluatedCode(testCode)

      countVals(code) shouldBe 2
      Array(4, 5) shouldEqual evaluated
    }

    it should "not Pre Evaluate non-lazy val" in new ToolboxTest {
      private val options = literalOptions(tmsOptions :+ "Pre Evaluate Option")

      val testCode: String =
        s"""$ToolboxTestImports
           |
           |val si = Some(1)
           |
           |tmsFor[Option[Int]]$options( si + Some(2) )
           |
           |""".stripMargin

      val (code, evaluated) = evaluatedCode(testCode)

      countVals(code) shouldBe 2
      evaluated shouldBe Some(3)
    }

    if (ScalaStringVersion >= "2.12") { // 2.11: even NON-TOOLBOX tests may not properly deal with it's lazy code outside macro (error: not found: bitmap$...)
      it should "Pre Evaluate lazy val" in new ToolboxTest {
        private val options = literalOptions(tmsOptions :+ "Pre Evaluate Option")

        val testCode: String =
          s"""$ToolboxTestImports
             |
             |lazy val lsi = Some(1)
             |
             |tmsFor[Option[Int]]$options( lsi + Some(2) )
             |
             |""".stripMargin

        val (code, evaluated) = evaluatedCode(testCode)

        countVals(code) shouldBe 3
        evaluated shouldBe Some(3)
      }
    }

    it should "postprocess single tts2 over single value of tms stack type and drop recursively 2 degenerate fors with WARN: tms has no sense to apply" in new ToolboxTest {
      val testCode: String =
        s"""$ToolboxTestImports
           |
           |tmsFor[Try[Option[Int]]]$options(tts2(Try(Some(3)))) // reasonably warns: there is no built fors in the macros transformation result
           |
           |""".stripMargin

      val (code, evaluated) = evaluatedCode(testCode)

      countSubstrings(code, """ap\(""") shouldBe 0
      evaluated shouldBe Try(Some(3))
    }

    it should "postprocess but not inline single pre evaluation when its usage is non-first enum" in new ToolboxTest {
      private val options = literalOptions(tmsOptions :+ "Pre Evaluate All Types")

      val testCode: String =
        s"""$ToolboxTestImports
           |
           |val si = Some(1)
           |val ti = Try(2)
           |
           |tmsFor[Try[Option[Int]]]$options {
           |  si + ti // stable vals are not pre evaluated and one extraction will be the first enum in for collector
           |  Some(3) + Try(4)
           |}
           |
           |""".stripMargin

      val (code, evaluated) = evaluatedCode(testCode)

      countVals(code) shouldBe 2 /* external si and ti vals */ + 2 /* pre evaluations of Some(3) and Try(4) */
      evaluated shouldBe Success(Some(7))
    }

    termsReusageTests("Pre Evaluate No Types")
    termsReusageTests("Pre Evaluate All Types")

    def termsReusageTests(preEvalOption: String): Unit = {
      val options = literalOptions(tmsOptions :+ preEvalOption)

      it should s"not reuse already extracted value of tms def, $preEvalOption" in new ToolboxTest {
        val testCode: String =
          s"""$ToolboxTestImports
             |
             |def i() = Some((1, 2))
             |
             |tmsFor[Option[Int]]$options {
             |  i()._1 + i()._2
             |}
             |
             |""".stripMargin

        val (code, evaluated) = evaluatedCode(testCode)

        countSubstrings(code, "valueOfSome\\$macro") shouldBe 4
        evaluated shouldBe Some(3)
      }

      it should s"not reuse already extracted value of tms var, $preEvalOption" in new ToolboxTest {
        val testCode: String =
          s"""$ToolboxTestImports
             |
             |var i = Some((1, 2))
             |
             |tmsFor[Option[Int]]$options {
             |  i._1 + i._2
             |}
             |
             |""".stripMargin

        val (code, evaluated) = evaluatedCode(testCode)

        countSubstrings(code, "valueOfSome\\$macro") shouldBe 4
        evaluated shouldBe Some(3)
      }

      it should s"reuse already extracted value of tms val, $preEvalOption" in new ToolboxTest {
        val testCode: String =
          s"""$ToolboxTestImports
             |
             |val i = Some((1, 2))
             |
             |tmsFor[Option[Int]]$options {
             |  i._1 + i._2
             |}
             |
             |""".stripMargin

        val (code, evaluated) = evaluatedCode(testCode)

        countSubstrings(code, "valueOfSome\\$macro") shouldBe 3
        evaluated shouldBe Some(3)
      }

      it should s"reuse stack of already extracted value of tms val, $preEvalOption" in new ToolboxTest {
        val testCode: String =
          s"""$ToolboxTestImports
             |
             |val i = Try(Some((1, 2)))
             |
             |tmsFor[Try[Option[Int]]]$options {
             |  i._1 + i._2
             |}
             |
             |""".stripMargin

        val (code, evaluated) = evaluatedCode(testCode)

        countSubstrings(code, "valueOfTry\\$macro") shouldBe 2
        countSubstrings(code, "valueOfSome\\$macro") shouldBe 3
        evaluated shouldBe Try(Some(3))
      }

      it should s"reuse partial stack of already extracted value of tms val, $preEvalOption" in new ToolboxTest {
        val testCode: String =
          s"""$ToolboxTestImports
             |
             |val i = Try(Some((1, 2)))
             |
             |tmsFor[Try[Option[Int]]]$options {
             |  i._1 + tts1(i).getOrElse((0, 0))._2
             |}
             |
             |""".stripMargin

        val (code, evaluated) = evaluatedCode(testCode)

        countSubstrings(code, "valueOfTry\\$macro") shouldBe 3
        countSubstrings(code, "valueOfSome\\$macro") shouldBe 2
        evaluated shouldBe Try(Some(3))
      }

      it should s"reuse stack of already extracted value of tms val and extend stack to inner value not used before, $preEvalOption" in new ToolboxTest {
        val testCode: String =
          s"""$ToolboxTestImports
             |
             |val i = Try(Some((1, 2)))
             |
             |tmsFor[Try[Option[Int]]]$options {
             |  tts1(i).getOrElse((0, 0))._1 + i._2
             |}
             |
             |""".stripMargin

        val (code, evaluated) = evaluatedCode(testCode)

        countSubstrings(code, "valueOfTry\\$macro") shouldBe 3
        countSubstrings(code, "valueOfSome\\$macro") shouldBe 2
        evaluated shouldBe Try(Some(3))
      }

      it should s"reuse already extracted value of tms object val, $preEvalOption" in new ToolboxTest {
        val testCode: String =
          s"""$ToolboxTestImports
             |
             |object O {
             |  val i = Some((1, 2))
             |}
             |
             |tmsFor[Option[Int]]$options {
             |  O.i._1 + O.i._2
             |}
             |
             |""".stripMargin

        val (code, evaluated) = evaluatedCode(testCode)

        countSubstrings(code, "valueOfSome\\$macro") shouldBe 3
        evaluated shouldBe Some(3)
      }

      it should s"reuse already extracted value of imported tms object val, $preEvalOption" in new ToolboxTest {
        val testCode: String =
          s"""$ToolboxTestImports
             |
             |object O {
             |  val i = Some((1, 2))
             |}
             |
             |tmsFor[Option[Int]]$options {
             |  import O.i
             |  O.i._1 + i._2 // the same Select(O, i) in tree of both
             |}
             |
             |""".stripMargin

        val (code, evaluated) = evaluatedCode(testCode)

        countSubstrings(code, "valueOfSome\\$macro") shouldBe 3
        evaluated shouldBe Some(3)
      }

      it should s"reuse already extracted value of tms inner object val, $preEvalOption" in new ToolboxTest {
        val testCode: String =
          s"""$ToolboxTestImports
             |
             |object O {
             |  object I {
             |    val i = Some((1, 2))
             |  }
             |}
             |
             |tmsFor[Option[Int]]$options {
             |  O.I.i._1 + O.I.i._2
             |}
             |
             |""".stripMargin

        val (code, evaluated) = evaluatedCode(testCode)

        countSubstrings(code, "valueOfSome\\$macro") shouldBe 3
        evaluated shouldBe Some(3)
      }

      if (ScalaStringVersion >= "2.12") { // 2.11: even NON-TOOLBOX tests may not properly deal with it's lazy code outside macro (error: not found: bitmap$...)
        // for 2.11: <toolbox>:12: error: not found: value i$lzy
        it should s"reuse already extracted value of tms lazy val, $preEvalOption" in new ToolboxTest {
          val testCode: String =
            s"""$ToolboxTestImports
               |
               |lazy val i: Option[(Int, Int)] = Some((1, 2))
               |
               |tmsFor[Option[Int]]$options {
               |  i._1 + i._2
               |}
               |
               |""".stripMargin

          val (code, evaluated) = evaluatedCode(testCode)

          countSubstrings(code, "valueOfOption\\$macro") shouldBe 3
          evaluated shouldBe Some(3)
        }

        // for 2.11: <toolbox>:13: error: i  is already defined as value i
        it should s"reuse already extracted value of tms object lazy val, $preEvalOption" in new ToolboxTest {
          val testCode: String =
            s"""$ToolboxTestImports
               |
               |object O {
               |  lazy val i: Option[(Int, Int)] = Some((1, 2))
               |}
               |
               |tmsFor[Option[Int]]$options {
               |  O.i._1 + O.i._2
               |}
               |
               |""".stripMargin

          val (code, evaluated) = evaluatedCode(testCode)

          countSubstrings(code, "valueOfOption\\$macro") shouldBe 3
          evaluated shouldBe Some(3)
        }
      }
    }

    it should "embed forsCodeView into output code" in new ToolboxTest {
      val testCode: String =
        s"""$ToolboxTestImports
           |
           |tmsFor[Option[Int]]("Embedded Fors Code View") {
           |  Some(1) + 1
           |}
           |
           |""".stripMargin

      val (code, evaluated) = evaluatedCode(testCode)

      code should include("val forsCodeView$")
      evaluated shouldBe Some(2)
    }

    it should "compile 'tms' macro variant: NON-TOOLBOX" in {
      import sands.sugar.tms.TransparentMonads._

      tms[Option[Int]](Some(0) + 1) shouldBe Some(1)
    }

    it should "compile 'tmsFor' macro variant: NON-TOOLBOX" in {
      import sands.sugar.tms.TransparentMonads._

      tmsFor[Option[Int]]("ND")(Some(0) + 1) shouldBe Some(1)
    }

    it should "compile 'transparentMonads' macro variant: NON-TOOLBOX" in {
      import sands.sugar.tms.TransparentMonads._

      transparentMonads[Option[Int]](Some(0) + 1) shouldBe Some(1)
    }

    it should "compile 'transparentMonadsFor' macro variant: NON-TOOLBOX" in {
      import sands.sugar.tms.TransparentMonads._

      transparentMonadsFor[Option[Int]]("ND")(Some(0) + 1) shouldBe Some(1)
    }

  }
}
