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
import sands.sugar.tms.TmsOptions.ForsStackForApplyParameterOption

import scala.tools.reflect.ToolBoxError
import scala.util.{Failure, Try}

/*
 * Created by Serhiy Shamshetdinov
 * at 11.12.2020 11:52
 */

class TmsForDefScopeUsageTest extends TmsTestBase {

  testWithForsStackWithPreEvaluationVariants(defScopeUsageTests)

  def defScopeUsageTests(tmsOptions: Seq[String]): Unit = {
    val options = literalOptions(tmsOptions)

    behavior of "tms macros Definitions Scope Usage with TmsOptions" + options

    it should "not fail when tms local val is used in root Fors Stack yield" in new ToolboxTest {
      val testCode: String =
        s"""$ToolboxTestImports
           |
           |tmsFor[Option[Int]]$options {
           |  val i: Int = 1
           |  i + Some(2)
           |}
           |""".stripMargin

      evaluateCode(testCode) shouldBe Some(3)
    }

    it should "fail when tms local val is used in root Fors Stack <- or preEval (with 'PEAT' option)" in new ToolboxTest {
      val testCode: String =
        s"""$ToolboxTestImports
           |
           |tmsFor[Option[Int]]$options {
           |  val i: Int = 1
           |  Some(i) + 2
           |}
           |""".stripMargin

      Try(evaluateCode(testCode)) should matchPattern {
        case Failure(ToolBoxError(msg, _)) if msg.startsWith(
          "reflective typecheck has failed: locally defined (in tmsCode) 'value i' may not be used") =>
      }
    }

    if (tmsOptions.contains[String](ForsStackForApplyParameterOption)) {
      it should "fail when tms local val is used in non-root Fors Stack yield (with 'FSFAP' option)" in new ToolboxTest {
        val testCode: String =
          s"""$ToolboxTestImports
             |
             |tmsFor[Option[Int]]$options {
             |  val i: Int = 1
             |  1 + (i + Some(2))
             |}
             |""".stripMargin

        Try(evaluateCode(testCode)) should matchPattern {
          case Failure(ToolBoxError(msg, _)) if msg.startsWith(
            "reflective typecheck has failed: locally defined (in tmsCode) 'value i' may not be used") =>
        }
      }

      it should "fail when tms local val is used in non-root Fors Stack <- or preEval (with 'FSFAP' and variants of 'PEAT' options)" in new ToolboxTest {
        val testCode: String =
          s"""$ToolboxTestImports
             |
             |tmsFor[Option[Int]]$options {
             |  val i: Int = 1
             |  1 + (2 + Some(i))
             |}
             |""".stripMargin

        Try(evaluateCode(testCode)) should matchPattern {
          case Failure(ToolBoxError(msg, _)) if msg.startsWith(
            "reflective typecheck has failed: locally defined (in tmsCode) 'value i' may not be used") =>
        }
      }

      it should "fail when defined class is created with tts parameter in non-root Fors Stack yield (with 'FSFAP' option)" in new ToolboxTest {
        val testCode: String =
          s"""$ToolboxTestImports
             |
             |tmsFor[Option[Int]]$options {
             |  class C(val i: Int)
             |
             |  1 + new C(Some(2)).i
             |}
             |""".stripMargin

        Try(evaluateCode(testCode)) should matchPattern {
          case Failure(ToolBoxError(msg, _)) if msg.startsWith(
            "reflective typecheck has failed: locally defined (in tmsCode) 'class C' may not be used") =>
        }
      }

      it should "fail when defined method is used with tts parameter in non-root Fors Stack yield (with 'FSFAP' option)" in new ToolboxTest {
        val testCode: String =
          s"""$ToolboxTestImports
             |
             |tmsFor[Option[Int]]$options {
             |  def m(i: Int) = i
             |
             |  1 + m(Some(1))
             |}
             |""".stripMargin

        Try(evaluateCode(testCode)) should matchPattern {
          case Failure(ToolBoxError(msg, _)) if msg.startsWith(
            "reflective typecheck has failed: locally defined (in tmsCode) 'method m' may not be used") =>
        }
      }

      it should "fail when defined object method is used with tts parameter in non-root Fors Stack yield (with 'FSFAP' option)" in new ToolboxTest {
        val testCode: String =
          s"""$ToolboxTestImports
             |
             |tmsFor[Option[Int]]$options {
             |  object O {
             |    def m(i: Int) = i
             |  }
             |
             |  1 + O.m(Some(1))
             |}
             |""".stripMargin

        Try(evaluateCode(testCode)) should matchPattern {
          case Failure(ToolBoxError(msg, _)) if msg.startsWith(
            "reflective typecheck has failed: locally defined (in tmsCode) 'object O' may not be used") =>
        }
      }
    }

    it should "not fail when the not used in tts block local val is defined in root or non-root Fors Stack yield (variants of 'FSFAP' option)" in new ToolboxTest {
      val testCode: String =
        s"""$ToolboxTestImports
           |
           |tmsFor[Option[Int]]$options {
           |  1 + ({
           |    val i: Int = 2
           |    i
           |  } + Some(3))
           |}
           |""".stripMargin

      evaluateCode(testCode) shouldBe Some(6)
    }

    it should "not fail when the not used in tts block local val is defined in root or non-root Fors Stack <- or preEval (variants of 'FSFAP' and 'PEAT' options)" in new ToolboxTest {
      val testCode: String =
        s"""$ToolboxTestImports
           |
           |tmsFor[Option[Int]]$options {
           |  1 + Some({
           |    val i: Int = 2
           |    i + Some(3)
           |  })
           |}
           |""".stripMargin

      evaluateCode(testCode) shouldBe Some(6)
    }


    it should "fail when defined var is used inside tts expression" in new ToolboxTest {
      val testCode: String =
        s"""$ToolboxTestImports
           |
           |tmsFor[Option[Int]]$options {
           |  var i = Some(0)
           |  i = Some(1)
           |  i + 2
           |}
           |""".stripMargin

      Try(evaluateCode(testCode)) should matchPattern {
        case Failure(ToolBoxError(msg, _)) if msg.startsWith(
          "reflective typecheck has failed: locally defined (in tmsCode) 'variable i' may not be used") =>
      }
    }

    it should "not fail when the not used in tts block local var is defined in root or non-root Fors Stack <- or preEval (variants of 'FSFAP' and 'PEAT' options)" in new ToolboxTest {
      val testCode: String =
        s"""$ToolboxTestImports
           |
           |tmsFor[Option[Int]]$options {
           |  1 + Some({
           |    var i: Int = 0
           |    i = 2
           |    i + Some(3)
           |  })
           |}
           |""".stripMargin

      evaluateCode(testCode) shouldBe Some(6)
    }

    if (ScalaStringVersion >= "2.12") {
      // for 2.11 the check for improper usage of lazy vals is skipped: compiler will emit related error.
      // see comment in sands.sugar.tms.TmsForExtraction.TmsExtractedTree.tmsExtractedTreePostprocessor
      it should "fail when defined lazy val is used inside tts expression" in new ToolboxTest {
        val testCode: String =
          s"""$ToolboxTestImports
             |
             |tmsFor[Option[Int]]$options {
             |  lazy val i = Some(1)
             |  i + 2
             |}
             |""".stripMargin

        Try(evaluateCode(testCode)) should matchPattern {
          case Failure(ToolBoxError(msg, _)) if msg.startsWith(
            "reflective typecheck has failed: locally defined (in tmsCode) 'lazy value i' may not be used") =>
        }
      }

      it should "not fail when the not used in tts block local lazy val is defined in root or non-root Fors Stack <- or preEval (variants of 'FSFAP' and 'PEAT' options)" in new ToolboxTest {
        val testCode: String =
          s"""$ToolboxTestImports
             |
             |tmsFor[Option[Int]]$options {
             |  1 + Some({
             |    lazy val i: Int = 2
             |    i + Some(3)
             |  })
             |}
             |""".stripMargin

        evaluateCode(testCode) shouldBe Some(6)
      }
    }

    it should "fail when defined method is used inside root Fors Stack tts expression" in new ToolboxTest {
      val testCode: String =
        s"""$ToolboxTestImports
           |
           |tmsFor[Option[Int]]$options {
           |  def m = Some(1)
           |  m + 2
           |}
           |""".stripMargin

      Try(evaluateCode(testCode)) should matchPattern {
        case Failure(ToolBoxError(msg, _)) if msg.startsWith(
          "reflective typecheck has failed: locally defined (in tmsCode) 'method m' may not be used") =>
      }
    }

    it should "not fail when the not used in tts block local method is defined in root or non-root Fors Stack <- or preEval (variants of 'FSFAP' and 'PEAT' options)" in new ToolboxTest {
      val testCode: String =
        s"""$ToolboxTestImports
           |
           |tmsFor[Option[Int]]$options {
           |  1 + Some({
           |    def m: Int = 2
           |    m + Some(3)
           |  })
           |}
           |""".stripMargin

      evaluateCode(testCode) shouldBe Some(6)
    }

    it should "fail when defined method parameter is used inside tts expression" in new ToolboxTest {
      val testCode: String =
        s"""$ToolboxTestImports
           |
           |tmsFor[Option[Int]]$options {
           |  def m(i: Int) = Some(i) + 1
           |  m(2)
           |}
           |""".stripMargin

      Try(evaluateCode(testCode)) should matchPattern {
        case Failure(ToolBoxError(msg, _)) if msg.startsWith(
          "reflective typecheck has failed: locally defined (in tmsCode) 'value i' may not be used") =>
      }
    }

    it should "not fail when the not used in tts block local method parameter is defined in root or non-root Fors Stack <- or preEval (variants of 'FSFAP' and 'PEAT' options)" in new ToolboxTest {
      val testCode: String =
        s"""$ToolboxTestImports
           |
           |tmsFor[Option[Int]]$options {
           |  1 + Some({
           |    def m(i: Int) = i + Some(2)
           |    m(4) + Some(3)
           |  })
           |}
           |""".stripMargin

      evaluateCode(testCode) shouldBe Some(10)
    }

    it should "fail when defined method type parameter is used inside tts expression" in new ToolboxTest {
      val testCode: String =
        s"""$ToolboxTestImports
           |
           |tmsFor[Option[Int]]$options {
           |  def m[T <: Int]() = 1 + Some(2.asInstanceOf[T])
           |  Some(3)
           |}
           |""".stripMargin

      Try(evaluateCode(testCode)) should matchPattern {
        case Failure(ToolBoxError(msg, _)) if msg.startsWith(
          "reflective typecheck has failed: locally defined (in tmsCode) 'type T' may not be used") =>
      }
    }

    it should "not fail when the not used in tts block local method type parameter is defined in root or non-root Fors Stack <- or preEval (variants of 'FSFAP' and 'PEAT' options)" in new ToolboxTest {
      val testCode: String =
        s"""$ToolboxTestImports
           |
           |tmsFor[Option[Int]]$options {
           |  1 + Some({
           |    def m[T <: Int]() = 2.asInstanceOf[T]
           |    2 + Some(3)
           |  })
           |}
           |""".stripMargin

      evaluateCode(testCode) shouldBe Some(6)
    }

    it should "fail when function parameter is used inside tts expression" in new ToolboxTest {
      val testCode: String =
        s"""$ToolboxTestImports
           |
           |tmsFor[Option[Int]]$options {
           |  ((i: Int) => Some(i) + 1)(2)
           |}
           |""".stripMargin

      Try(evaluateCode(testCode)) should matchPattern {
        case Failure(ToolBoxError(msg, _)) if msg.startsWith(
          "reflective typecheck has failed: locally defined (in tmsCode) 'value i' may not be used") =>
      }
    }

    it should "not fail when the not used in tts block local function parameter is defined in root or non-root Fors Stack <- or preEval (variants of 'FSFAP' and 'PEAT' options)" in new ToolboxTest {
      val testCode: String =
        s"""$ToolboxTestImports
           |
           |tmsFor[Option[Int]]$options {
           |  1 + Some({
           |    ((i: Int) => Some(2) + i)(3) + Some(4)
           |  })
           |}
           |""".stripMargin

      evaluateCode(testCode) shouldBe Some(10)
    }

    it should "fail when function with placeholder parameter is used inside tts expression" in new ToolboxTest {
      val testCode: String =
        s"""$ToolboxTestImports
           |
           |tmsFor[Option[Int]]$options {
           |  (Some((_: Int)) + 1)(2)
           |}
           |""".stripMargin

      Try(evaluateCode(testCode)) should matchPattern {
        case Failure(ToolBoxError(msg, _)) if msg.startsWith(
          "reflective typecheck has failed: locally defined (in tmsCode) 'value x$1' may not be used") =>
      }
    }

    it should "not fail when the not used in tts block local function with placeholder parameter is defined in root or non-root Fors Stack <- or preEval (variants of 'FSFAP' and 'PEAT' options)" in new ToolboxTest {
      val testCode: String =
        s"""$ToolboxTestImports
           |
           |tmsFor[Option[Int]]$options {
           |  1 + Some({
           |    (Some(2) + (_: Int))(3) + Some(4)
           |  })
           |}
           |""".stripMargin

      evaluateCode(testCode) shouldBe Some(10)
    }

    it should "fail when defined case bind val is used inside tts expression" in new ToolboxTest {
      val testCode: String =
        s"""$ToolboxTestImports
           |
           |tmsFor[Option[Int]]$options {
           |  5 match {
           |    case i: Int => Some(i) + 1
           |  }
           |}
           |""".stripMargin

      Try(evaluateCode(testCode)) should matchPattern {
        case Failure(ToolBoxError(msg, _)) if msg.startsWith(
          "reflective typecheck has failed: locally defined (in tmsCode) 'value i' may not be used") =>
      }
    }

    it should "not fail when the not used in tts block local case bind val is defined in root or non-root Fors Stack <- or preEval (variants of 'FSFAP' and 'PEAT' options)" in new ToolboxTest {
      val testCode: String =
        s"""$ToolboxTestImports
           |
           |tmsFor[Option[Int]]$options {
           |  1 + Some({
           |    0 match {
           |      case i: Int => i
           |    }
           |    2 + Some(3)
           |  })
           |}
           |""".stripMargin

      evaluateCode(testCode) shouldBe Some(6)
    }

    it should "fail when defined case unapply bind val is used inside tts expression" in new ToolboxTest {
      val testCode: String =
        s"""$ToolboxTestImports
           |
           |tmsFor[Option[Int]]$options {
           |  Some(5) match {
           |    case Some(i: Int) => Some(i) + 1
           |  }
           |}
           |""".stripMargin

      Try(evaluateCode(testCode)) should matchPattern {
        case Failure(ToolBoxError(msg, _)) if msg.startsWith(
          "reflective typecheck has failed: locally defined (in tmsCode) 'value i' may not be used") =>
      }
    }

    it should "not fail when the not used in tts block local case unapply bind val is defined in root or non-root Fors Stack <- or preEval (variants of 'FSFAP' and 'PEAT' options)" in new ToolboxTest {
      val testCode: String =
        s"""$ToolboxTestImports
           |
           |tmsFor[Option[Int]]$options {
           |  1 + Some({
           |    Some(0) match {
           |      case Some(i: Int) => i
           |    }
           |    2 + Some(3)
           |  })
           |}
           |""".stripMargin

      evaluateCode(testCode) shouldBe Some(6)
    }

    it should "fail when defined object is used inside tts expression" in new ToolboxTest {
      val testCode: String =
        s"""$ToolboxTestImports
           |
           |tmsFor[Option[Int]]$options {
           |  object O {
           |    val i = 3
           |  }
           |  Some(O).i
           |}
           |""".stripMargin

      Try(evaluateCode(testCode)) should matchPattern {
        case Failure(ToolBoxError(msg, _)) if msg.startsWith(
          "reflective typecheck has failed: locally defined (in tmsCode) 'object O' may not be used") =>
      }
    }

    it should "not fail when the not used in tts block local object is defined in root or non-root Fors Stack <- or preEval (variants of 'FSFAP' and 'PEAT' options)" in new ToolboxTest {
      val testCode: String =
        s"""$ToolboxTestImports
           |
           |tmsFor[Option[Int]]$options {
           |  1 + Some({
           |    object O
           |    O.toString
           |    2 + Some(3)
           |  })
           |}
           |""".stripMargin

      evaluateCode(testCode) shouldBe Some(6)
    }

    it should "fail when defined object field is used inside tts expression" in new ToolboxTest {
      val testCode: String =
        s"""$ToolboxTestImports
           |
           |tmsFor[Option[Int]]$options {
           |  object O {
           |    val i = Some(2)
           |  }
           |  O.i + 3
           |}
           |""".stripMargin

      Try(evaluateCode(testCode)) should matchPattern {
        case Failure(ToolBoxError(msg, _)) if msg.startsWith(
          "reflective typecheck has failed: locally defined (in tmsCode) 'value i' may not be used") =>
      }
    }

    it should "not fail when the not used in tts block local object field is defined in root or non-root Fors Stack <- or preEval (variants of 'FSFAP' and 'PEAT' options)" in new ToolboxTest {
      val testCode: String =
        s"""$ToolboxTestImports
           |
           |tmsFor[Option[Int]]$options {
           |  1 + Some({
           |    object O {
           |      val i: Int = 2
           |      i + 1
           |    }
           |    2 + Some(3)
           |  })
           |}
           |""".stripMargin

      evaluateCode(testCode) shouldBe Some(6)
    }

    it should "fail when defined class is used inside tts expression" in new ToolboxTest {
      val testCode: String =
        s"""$ToolboxTestImports
           |
           |tmsFor[Option[Int]]$options {
           |  class C() {
           |    val i = 2
           |  }
           |  Some(new C()).i
           |}
           |""".stripMargin

      Try(evaluateCode(testCode)) should matchPattern {
        case Failure(ToolBoxError(msg, _)) if msg.startsWith(
          "reflective typecheck has failed: locally defined (in tmsCode) 'class C' may not be used") =>
      }
    }

    it should "not fail when the not used in tts block local class is defined in root or non-root Fors Stack <- or preEval (variants of 'FSFAP' and 'PEAT' options)" in new ToolboxTest {
      val testCode: String =
        s"""$ToolboxTestImports
           |
           |tmsFor[Option[Int]]$options {
           |  1 + Some({
           |    class C()
           |    new C().toString
           |    2 + Some(3)
           |  })
           |}
           |""".stripMargin

      evaluateCode(testCode) shouldBe Some(6)
    }

    it should "fail when defined class field is used inside tts expression" in new ToolboxTest {
      val testCode: String =
        s"""$ToolboxTestImports
           |
           |tmsFor[Option[Int]]$options {
           |  class C() {
           |    val i = Some(3)
           |  }
           |  new C().i + 1
           |}
           |""".stripMargin

      Try(evaluateCode(testCode)) should matchPattern {
        case Failure(ToolBoxError(msg, _)) if msg.startsWith(
          "reflective typecheck has failed: locally defined (in tmsCode) 'value i' may not be used") =>
      }
    }

    it should "not fail when the not used in tts block local class field is defined in root or non-root Fors Stack <- or preEval (variants of 'FSFAP' and 'PEAT' options)" in new ToolboxTest {
      val testCode: String =
        s"""$ToolboxTestImports
           |
           |tmsFor[Option[Int]]$options {
           |  1 + Some({
           |    class C() {
           |      val i = 2
           |      i + 1
           |    }
           |    2 + Some(3)
           |  })
           |}
           |""".stripMargin

      evaluateCode(testCode) shouldBe Some(6)
    }

    it should "fail when defined class parameter is used inside tts expression" in new ToolboxTest {
      val testCode: String =
        s"""$ToolboxTestImports
           |
           |tmsFor[Option[Int]]$options {
           |  class C(i: Int) {
           |    Some(i) + 1
           |  }
           |  new C(3)
           |}
           |""".stripMargin

      Try(evaluateCode(testCode)) should matchPattern {
        case Failure(ToolBoxError(msg, _)) if msg.startsWith(
          "reflective typecheck has failed: locally defined (in tmsCode) 'value i' may not be used") =>
      }
    }

    it should "not fail when the not used in tts block local class parameter is defined in root or non-root Fors Stack <- or preEval (variants of 'FSFAP' and 'PEAT' options)" in new ToolboxTest {
      val testCode: String =
        s"""$ToolboxTestImports
           |
           |tmsFor[Option[Int]]$options {
           |  1 + Some({
           |    class C(val i: Int) {
           |      def j = i
           |    }
           |    2 + Some(3)
           |  })
           |}
           |""".stripMargin

      evaluateCode(testCode) shouldBe Some(6)
    }

    it should "fail when defined class type parameter is used inside tts expression" in new ToolboxTest {
      val testCode: String =
        s"""$ToolboxTestImports
           |
           |tmsFor[Option[Int]]$options {
           |  class C[T <: Int]() {
           |    1 + Some(2.asInstanceOf[T])
           |  }
           |  Some(3)
           |}
           |""".stripMargin

      Try(evaluateCode(testCode)) should matchPattern {
        case Failure(ToolBoxError(msg, _)) if msg.startsWith(
          "reflective typecheck has failed: locally defined (in tmsCode) 'type T' may not be used") =>
      }
    }

    it should "not fail when the not used in tts block local class type parameter is defined in root or non-root Fors Stack <- or preEval (variants of 'FSFAP' and 'PEAT' options)" in new ToolboxTest {
      val testCode: String =
        s"""$ToolboxTestImports
           |
           |tmsFor[Option[Int]]$options {
           |  1 + Some({
           |    class C[T <: Int]() {
           |      def j = 2.asInstanceOf[T]
           |    }
           |    2 + Some(3)
           |  })
           |}
           |""".stripMargin

      evaluateCode(testCode) shouldBe Some(6)
    }

    // self is always substituted by class (here: TestTrait.this, second test: $anon.this - inlined) and is never used as real val
    it should "fail when self val is used inside tts expression" in new ToolboxTest {
      val testCode: String =
        s"""$ToolboxTestImports
           |
           |trait TestTrait { def i: Int }
           |
           |tmsFor[Option[AnyRef]]$options {
           |  new TestTrait { self =>
           |    def i: Int = 1
           |    Some(self).i
           |  }
           |}
           |""".stripMargin

      Try(evaluateCode(testCode)) should matchPattern {
        case Failure(ToolBoxError(msg, _)) if msg.startsWith(
          "reflective typecheck has failed: locally defined (in tmsCode) '<$anon: TestTrait>' may not be used") =>
      }
    }

    it should "not fail when the not used in tts block local self val is defined in root or non-root Fors Stack <- or preEval (variants of 'FSFAP' and 'PEAT' options)" in new ToolboxTest {
      val testCode: String =
        s"""$ToolboxTestImports
           |
           |class C(val i: Int)
           |
           |tmsFor[Option[Int]]$options {
           |  1 + Some({
           |    trait TestTrait { self: C =>
           |      def s = self.toString
           |    }
           |    2 + Some(3)
           |  })
           |}
           |""".stripMargin

      evaluateCode(testCode) shouldBe Some(6)
    }

    it should "fail when early definition val is used inside tts expression" in new ToolboxTest {
      val testCode: String =
        s"""$ToolboxTestImports
           |
           |trait TestTrait
           |
           |tmsFor[Option[AnyRef]]$options {
           |  object O extends {
           |    val e: Int = 2
           |  } with TestTrait {
           |    Some(e) + 1
           |  }
           |}
           |""".stripMargin

      Try(evaluateCode(testCode)) should matchPattern {
        case Failure(ToolBoxError(msg, _)) if msg.startsWith(
          "reflective typecheck has failed: locally defined (in tmsCode) 'value e' may not be used") =>
      }
    }

    it should "not fail when the not used in tts block local early definition val is defined in root or non-root Fors Stack <- or preEval (variants of 'FSFAP' and 'PEAT' options)" in new ToolboxTest {
      val testCode: String =
        s"""$ToolboxTestImports
           |
           |trait TestTrait
           |
           |tmsFor[Option[Int]]$options {
           |  1 + Some({
           |    object O extends {
           |      val e: Int = 2
           |    } with TestTrait {
           |      e + 1
           |    }
           |    O.e + Some(3)
           |  })
           |}
           |""".stripMargin

      evaluateCode(testCode) shouldBe Some(6)
    }
  }
}
