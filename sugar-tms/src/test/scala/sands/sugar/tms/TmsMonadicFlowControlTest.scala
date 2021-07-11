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
import sands.sugar.tms.TmsOptions.DefaultMfOptions

import scala.tools.reflect.ToolBoxError
import scala.util.{Failure, Success, Try}

/*
 * Created by Serhiy Shamshetdinov
 * at 08.05.2021 12:52
 */

class TmsMonadicFlowControlTest extends TmsTestBase {

  generalMonadicFlowTests(SimpleForsStackOptions)

  testWithForsStackWithPreEvaluationVariants(monadicFlowTests)

  // should be run with SimpleForsStackOptions
  def generalMonadicFlowTests(stringOptions: Seq[String]): Unit = {
    val options = literalOptions(stringOptions)

    behavior of "tms Monadic Flow General Behaviour with TmsOptions" + options

    it should "fail when first non-monadic expression type is not found in the Types Stack" in new ToolboxTest {
      val testCode: String =
        s"""$ToolboxTestImports
           |
           |mfcFor[Option[Int]]$options {
           |  identity(1) // identity: to skip warning
           |  2
           |}
           |
           |""".stripMargin

      Try(evaluateCode(testCode)) should matchPattern {
        case Failure(ToolBoxError(msg, _)) if msg.contains(
          "Monadic Flow Type Int detected by the first statement does not conform to any of the Fors Stack types [Option]") =>
      }
    }

    it should "fail when first monadic expression type is not found in the Types Stack" in new ToolboxTest {
      val testCode: String =
        s"""$ToolboxTestImports
           |
           |mfcFor[Option[Int]]$options {
           |  Try(1)
           |  Try(2)
           |}
           |
           |""".stripMargin

      Try(evaluateCode(testCode)) should matchPattern {
        case Failure(ToolBoxError(msg, _)) if msg.contains(
          "Monadic Flow Type scala.util.Try[Int] detected by the first statement does not conform to any of the Fors Stack types [Option]") =>
      }
    }

    it should "fail when first val non-monadic type is not found in the Types Stack" in new ToolboxTest {
      val testCode: String =
        s"""$ToolboxTestImports
           |
           |mfcFor[Option[Int]]$options {
           |  val i = 1
           |  i
           |}
           |
           |""".stripMargin

      Try(evaluateCode(testCode)) should matchPattern {
        case Failure(ToolBoxError(msg, _)) if msg.contains(
          "Monadic Flow Type Int detected by the first statement does not conform to any of the Fors Stack types [Option]") =>
      }
    }

    it should "fail when first val monadic type is not found in the Types Stack" in new ToolboxTest {
      val testCode: String =
        s"""$ToolboxTestImports
           |
           |mfcFor[Option[Int]]$options {
           |  val i = Try(1)
           |  2
           |}
           |
           |""".stripMargin

      Try(evaluateCode(testCode)) should matchPattern {
        case Failure(ToolBoxError(msg, _)) if msg.contains(
          "Monadic Flow Type scala.util.Try[Int] detected by the first statement does not conform to any of the Fors Stack types [Option]") =>
      }
    }

    it should "fail when first statement of correct monadic type is lazy val" in new ToolboxTest {
      val testCode: String =
        s"""$ToolboxTestImports
           |
           |mfcFor[Option[Int]]$options {
           |  lazy val i = Some(1)
           |  2
           |}
           |
           |""".stripMargin

      Try(evaluateCode(testCode)) should matchPattern {
        case Failure(ToolBoxError(msg, _)) if msg.contains("invalid Monadic Flow Control statement") =>
      }
    }

    it should "fail when first statement of correct monadic type is var" in new ToolboxTest {
      val testCode: String =
        s"""$ToolboxTestImports
           |
           |mfcFor[Option[Int]]$options {
           |  var i = Some(1)
           |  2
           |}
           |
           |""".stripMargin

      Try(evaluateCode(testCode)) should matchPattern {
        case Failure(ToolBoxError(msg, _)) if msg.contains("invalid Monadic Flow Control statement") =>
      }
    }

    it should "fail when first statement of correct monadic type is def def" in new ToolboxTest {
      val testCode: String =
        s"""$ToolboxTestImports
           |
           |mfcFor[Option[Int]]$options {
           |  def i = Some(1)
           |  i
           |}
           |
           |""".stripMargin

      Try(evaluateCode(testCode)) should matchPattern {
        case Failure(ToolBoxError(msg, _)) if msg.contains("invalid Monadic Flow Control statement") =>
      }
    }

    it should "fail when first statement is type def" in new ToolboxTest {
      val testCode: String =
        s"""$ToolboxTestImports
           |
           |mfcFor[Option[Int]]$options {
           |  type I = Option[Int]
           |  val i: I = Some(1)
           |}
           |
           |""".stripMargin

      Try(evaluateCode(testCode)) should matchPattern {
        case Failure(ToolBoxError(msg, _)) if msg.contains("invalid Monadic Flow Control statement") =>
      }
    }

    it should "fail when first statement is class def" in new ToolboxTest {
      val testCode: String =
        s"""$ToolboxTestImports
           |
           |mfcFor[Option[Int]]$options {
           |  class I
           |  new I
           |}
           |
           |""".stripMargin

      Try(evaluateCode(testCode)) should matchPattern {
        case Failure(ToolBoxError(msg, _)) if msg.contains("invalid Monadic Flow Control statement") =>
      }
    }

    it should "fail when first statement is trait def" in new ToolboxTest {
      val testCode: String =
        s"""$ToolboxTestImports
           |
           |mfcFor[Option[Int]]$options {
           |  trait I
           |  new I {}
           |}
           |
           |""".stripMargin

      Try(evaluateCode(testCode)) should matchPattern {
        case Failure(ToolBoxError(msg, _)) if msg.contains("invalid Monadic Flow Control statement") =>
      }
    }

    it should "fail when first statement is object def" in new ToolboxTest {
      val testCode: String =
        s"""$ToolboxTestImports
           |
           |mfcFor[Option[Int]]$options {
           |  object I
           |  2
           |}
           |
           |""".stripMargin

      Try(evaluateCode(testCode)) should matchPattern {
        case Failure(ToolBoxError(msg, _)) if msg.contains("invalid Monadic Flow Control statement") =>
      }
    }

    it should "fail when the yield part def definition is accessed from preceding mf val def init" in new ToolboxTest {
      val testCode: String =
        s"""$ToolboxTestImports
           |
           |mfcFor[Option[Int]]$options {
           |  val i = Some(j)
           |
           |  def j = 1
           |  i + 2
           |}
           |
           |""".stripMargin

      Try(evaluateCode(testCode)) should matchPattern {
        case Failure(ToolBoxError(msg, _)) if msg.startsWith(
          "reflective typecheck has failed: locally defined (in tmsCode) 'method j' may not be used") =>
      }
    }

    it should "fail when the yield part def definition is accessed from preceding mf expression" in new ToolboxTest {
      val testCode: String =
        s"""$ToolboxTestImports
           |
           |mfcFor[Option[Int]]$options {
           |  Some(j)
           |
           |  def j = 1
           |  2
           |}
           |
           |""".stripMargin

      Try(evaluateCode(testCode)) should matchPattern {
        case Failure(ToolBoxError(msg, _)) if msg.startsWith(
          "reflective typecheck has failed: locally defined (in tmsCode) 'method j' may not be used") =>
      }
    }

    it should "accept single monadic flow type expression" in new ToolboxTest {
      val testCode: String =
        s"""$ToolboxTestImports
           |
           |mfcFor[Option[Int]]$options( Some[Int](Some(1)) )
           |
           |""".stripMargin

      evaluateCode(testCode) shouldBe Some(1)
    }

    it should "accept monadic flow type val as a single statement yielding Unit value" in new ToolboxTest {
      val testCode: String =
        s"""$ToolboxTestImports
           |
           |mfcFor[Option[Unit]]$options {
           |  val mf1 = Some(1) // mf type
           |}
           |
           |""".stripMargin

      evaluateCode(testCode) shouldBe Some({})
    }

    it should "accept monadic flow type val as the last statement yielding Unit value" in new ToolboxTest {
      val testCode: String =
        s"""$ToolboxTestImports
           |
           |mfcFor[Option[Unit]]$options {
           |  val mf1 = Some(1) // mf type
           |  val mf2 = Some(2)
           |}
           |
           |""".stripMargin

      evaluateCode(testCode) shouldBe Some({})
    }

    it should "accept non-monadic flow type val definition as the single yield statement yielding Unit value (adds Unit as last expression to yield block)" in new ToolboxTest {
      val testCode: String =
        s"""$ToolboxTestImports
           |
           |mfcFor[Option[Unit]]$options {
           |  val mf = Some(1) // mf type
           |  val nonMf = 3
           |}
           |
           |""".stripMargin

      evaluateCode(testCode) shouldBe Some({})
    }

    if (ScalaStringVersion >= "2.12") {
      // for 2.11: scala.tools.reflect.ToolBoxError: reflective compilation has failed: missing parameter type ???
      it should "accept lazy val definition as the single yield statement yielding Unit value (adds Unit as last expression to yield block)" in new ToolboxTest {
        val testCode: String =
          s"""$ToolboxTestImports
             |
             |mfcFor[Option[Unit]]$options {
             |  val mf = Some(1) // mf type
             |  lazy val nonMf = 3
             |}
             |
             |""".stripMargin

        evaluateCode(testCode) shouldBe Some({})
      }
    } else {
      it should "accept lazy val definition as the single yield statement yielding Unit value (adds Unit as last expression to yield block): NON-TOOLBOX" in {
        import sands.sugar.tms.TransparentMonads._

        mfcFor[Option[Unit]]() {
          val mf = Some(1) // mf type
          lazy val nonMf = 3
        } shouldBe Some({})
      }
    }

    it should "accept var definition as the single yield statement yielding Unit value (adds Unit as last expression to yield block)" in new ToolboxTest {
      val testCode: String =
        s"""$ToolboxTestImports
           |
           |mfcFor[Option[Unit]]$options {
           |  val mf = Some(1) // mf type
           |  var nonMf = 3
           |}
           |
           |""".stripMargin

      evaluateCode(testCode) shouldBe Some({})
    }

    it should "accept def definition as the single yield statement yielding Unit value (adds Unit as last expression to yield block)" in new ToolboxTest {
      val testCode: String =
        s"""$ToolboxTestImports
           |
           |mfcFor[Option[Unit]]$options {
           |  val mf = Some(1) // mf type
           |  def nonMf = 3
           |}
           |
           |""".stripMargin

      evaluateCode(testCode) shouldBe Some({})
    }

    it should "accept object definition as the single yield statement yielding Unit value (adds Unit as last expression to yield block)" in new ToolboxTest {
      val testCode: String =
        s"""$ToolboxTestImports
           |
           |mfcFor[Option[Unit]]$options {
           |  val mf = Some(1) // mf type
           |  object O
           |}
           |
           |""".stripMargin

      evaluateCode(testCode) shouldBe Some({})
    }

    it should "accept type definition as the single yield statement yielding Unit value (adds Unit as last expression to yield block)" in new ToolboxTest {
      val testCode: String =
        s"""$ToolboxTestImports
           |
           |mfcFor[Option[Unit]]$options {
           |  val mf = Some(1) // mf type
           |  type S = Some[Int]
           |}
           |
           |""".stripMargin

      evaluateCode(testCode) shouldBe Some({})
    }

    it should "accept class definition as the single yield statement yielding Unit value (adds Unit as last expression to yield block)" in new ToolboxTest {
      val testCode: String =
        s"""$ToolboxTestImports
           |
           |mfcFor[Option[Unit]]$options {
           |  val mf = Some(1) // mf type
           |  class C
           |}
           |
           |""".stripMargin

      evaluateCode(testCode) shouldBe Some({})
    }

    it should "accept trait definition as the single yield statement yielding Unit value (adds Unit as last expression to yield block)" in new ToolboxTest {
      val testCode: String =
        s"""$ToolboxTestImports
           |
           |mfcFor[Option[Unit]]$options {
           |  val mf = Some(1) // mf type
           |  trait C
           |}
           |
           |""".stripMargin

      evaluateCode(testCode) shouldBe Some({})
    }

    it should "accept non-monadic flow type val definition as the last yield statement yielding Unit value (adds Unit as last expression to yield block)" in new ToolboxTest {
      val testCode: String =
        s"""$ToolboxTestImports
           |
           |mfcFor[Option[Unit]]$options {
           |  val mf = Some(1) // mf type
           |  identity(2) // identity: to skip warning
           |  val nonMf = 3
           |}
           |
           |""".stripMargin

      evaluateCode(testCode) shouldBe Some({})
    }

    if (ScalaStringVersion >= "2.12") {
      // for 2.11: scala.tools.reflect.ToolBoxError: reflective compilation has failed: missing parameter type ???
      it should "accept lazy val definition as the last yield statement yielding Unit value (adds Unit as last expression to yield block)" in new ToolboxTest {
        val testCode: String =
          s"""$ToolboxTestImports
             |
             |mfcFor[Option[Unit]]$options {
             |  val mf = Some(1) // mf type
             |  identity(2) // identity: to skip warning
             |  lazy val nonMf = 3
             |}
             |
             |""".stripMargin

        evaluateCode(testCode) shouldBe Some({})
      }
    } else {
      it should "accept lazy val definition as the last yield statement yielding Unit value (adds Unit as last expression to yield block): NON-TOOLBOX" in {
        import sands.sugar.tms.TransparentMonads._

        mfcFor[Option[Unit]]() {
          val mf = Some(1) // mf type
          identity(2) // to suppress warning
          lazy val nonMf = 3
        } shouldBe Some({})
      }
    }

    it should "accept var definition as the last yield statement yielding Unit value (adds Unit as last expression to yield block)" in new ToolboxTest {
      val testCode: String =
        s"""$ToolboxTestImports
           |
           |mfcFor[Option[Unit]]$options {
           |  val mf = Some(1) // mf type
           |  identity(2) // to suppress warning
           |  var nonMf = 3
           |}
           |
           |""".stripMargin

      evaluateCode(testCode) shouldBe Some({})
    }

    it should "accept def definition as the last yield statement yielding Unit value (adds Unit as last expression to yield block)" in new ToolboxTest {
      val testCode: String =
        s"""$ToolboxTestImports
           |
           |mfcFor[Option[Unit]]$options {
           |  val mf = Some(1) // mf type
           |  identity(2) // identity: to skip warning
           |  def nonMf = 3
           |}
           |
           |""".stripMargin

      evaluateCode(testCode) shouldBe Some({})
    }

    it should "accept object definition as the last yield statement yielding Unit value (adds Unit as last expression to yield block)" in new ToolboxTest {
      val testCode: String =
        s"""$ToolboxTestImports
           |
           |trait OI
           |
           |mfcFor[Option[Unit]]$options {
           |  val mf = Some(1) // mf type
           |  identity(2) // identity: to skip warning
           |  object O extends OI
           |}
           |
           |""".stripMargin

      evaluateCode(testCode) shouldBe Some({})
    }

    it should "accept type definition as the last yield statement yielding Unit value (adds Unit as last expression to yield block)" in new ToolboxTest {
      val testCode: String =
        s"""$ToolboxTestImports
           |
           |mfcFor[Option[Unit]]$options {
           |  val mf = Some(1) // mf type
           |  identity(2) // identity: to skip warning
           |  type S = Some[Int]
           |}
           |
           |""".stripMargin

      evaluateCode(testCode) shouldBe Some({})
    }

    it should "accept class definition as the last yield statement yielding Unit value (adds Unit as last expression to yield block)" in new ToolboxTest {
      val testCode: String =
        s"""$ToolboxTestImports
           |
           |mfcFor[Option[Unit]]$options {
           |  val mf = Some(1) // mf type
           |  identity(2) // identity: to skip warning
           |  class C
           |}
           |
           |""".stripMargin

      evaluateCode(testCode) shouldBe Some({})
    }

    it should "accept trait definition as the last yield statement yielding Unit value (adds Unit as last expression to yield block)" in new ToolboxTest {
      val testCode: String =
        s"""$ToolboxTestImports
           |
           |mfcFor[Option[Unit]]$options {
           |  val mf = Some(1) // mf type
           |  identity(2) // identity: to skip warning
           |  trait C
           |}
           |
           |""".stripMargin

      evaluateCode(testCode) shouldBe Some({})
    }

    it should "accept mixed monadic flow type & other type vals that are not used in the result and inline mf vals" in new ToolboxTest {
      val testCode: String =
        s"""$ToolboxTestImports
           |
           |mfcFor[Option[Int]]$options {
           |  val mf1 = Some(1) // mf type
           |  val nonMf1 = 2
           |  val mf2 = Some(3)
           |  val nonMf2 = 4
           |  val mf3 = Some(5)
           |  0
           |}
           |
           |""".stripMargin

      val (code, evaluated) = evaluatedCode(testCode)

      countSubstrings(code, "mf") shouldBe 7 // in the output non-fors Code View
      countSubstrings(code, "nonMf") shouldBe 6 // in the output non-fors Code View
      countVals(code) shouldBe 2 // only 2 nonMf vals, mf vals are postprocessed and are inlined

      evaluated shouldBe Some(0)
    }

    it should "group several non-monadic flow type expressions to the block separating vals and should give fresh names to wildcard for-enums" in new ToolboxTest {
      val testCode: String =
        s"""$ToolboxTestImports
           |
           |mfcFor[Option[Int]]$options {
           |  Some(1)     // mf type
           |  identity(2) // identity: to skip warning
           |  val i = 3
           |  identity(4) // identity: to skip warning
           |  identity(5) // identity: to skip warning
           |  val j = Some(6)
           |  i + j
           |}
           |
           |""".stripMargin

      val (code, evaluated) = evaluatedCode(testCode)

      countSubstrings(code, "wcMf") shouldBe 3
      countSubstrings(code, "wcNonMf") shouldBe 6
      evaluated shouldBe Some(9)
    }

    it should "accept several non-monadic flow type expressions with defs grouped to block" in new ToolboxTest {
      val testCode: String =
        s"""$ToolboxTestImports
           |
           |mfcFor[Option[Int]]$options {
           |  Some(1); // mf type
           |  {
           |    type T = Int
           |    var v: T = 3
           |    def i: T = v
           |    i
           |  }
           |  val i = Some(4)
           |  identity(5); // identity: to skip warning
           |  {
           |    trait T
           |    class C extends T
           |    new C
           |    object O
           |  }
           |  val j = Some(6)
           |  i + j
           |}
           |
           |""".stripMargin

      evaluateCode(testCode) shouldBe Some(10)
    }

    it should "flatMap last Monadic Flow Type expression which type (like the type of the full input) is inferred as Any" in new ToolboxTest {
      val testCode: String =
        s"""$ToolboxTestImports
           |
           |mfcFor[Option[Int]]$options {
           |  Some(1)
           |  try Some(2) finally {}
           |}
           |
           |""".stripMargin

      val (code, evaluated) = evaluatedCode(testCode)

      countSubstrings(code, "flatMappedValueOfSome") shouldBe 2
      evaluated shouldBe Some(2)
    }

    it should "split statements with try and infer types correctly" in new ToolboxTest {
      val testCode: String =
        s"""$ToolboxTestImports
           |
           |mfcFor[Option[Int]]$options {
           |  val i = try Some(1) finally {} // mf type
           |
           |  val j = try 2 finally {} // to suppress warn
           |  try 3 finally {}
           |  i + j
           |}
           |
           |""".stripMargin

      evaluateCode(testCode) shouldBe Some(3)
    }

    it should "accept another mf macro inside mf type val init" in new ToolboxTest {
      val testCode: String =
        s"""$ToolboxTestImports
           |
           |mfcFor[Option[Int]]$options {
           |  val ms = mfcFor[Option[String]]$options {
           |    val os = Some("test")
           |    os.concat("done")
           |  }
           |
           |  ms.length
           |}
           |
           |""".stripMargin

      evaluateCode(testCode) shouldBe Some(8)
    }

    it should "accept another mf macro as non-mf type expression" in new ToolboxTest {
      val testCode: String =
        s"""$ToolboxTestImports
           |
           |mfcFor[Option[Int]]$options {
           |  val ss = Some("test")
           |  mfcFor[Try[String]]$options {
           |    val ts = Try("test")
           |    ts.concat("done")
           |  }
           |  Some(0)
           |
           |  ss.length
           |}
           |
           |""".stripMargin

      evaluateCode(testCode) shouldBe Some(4)
    }

    it should "accept another mf macro in yield" in new ToolboxTest {
      val testCode: String =
        s"""$ToolboxTestImports
           |
           |mfcFor[Option[Int]]$options {
           |  val ss = Some("test")
           |
           |  mfcFor[Try[String]]$options {
           |    val ts = Try("test")
           |    ts.concat("done")
           |  }.get.length
           |}
           |
           |""".stripMargin

      evaluateCode(testCode) shouldBe Some(8)
    }


    it should "pre evaluate no types" in new ToolboxTest {
      private val options = literalOptions(stringOptions :+ "Pre Evaluate No Types")

      val testCode: String =
        s"""$ToolboxTestImports
           |
           |mfcFor[Try[Option[Int]]]$options {
           |  val t1 = Try(1)
           |  Try(0) // to prevent single Try pre eval optimization
           |
           |  Some(0) + 0 // to prevent single Option pre eval optimization
           |  Some(t1 + 2) + 3
           |}
           |
           |""".stripMargin

      val (code, evaluated) = evaluatedCode(testCode)

      countSubstrings(code, "val t1") shouldBe 0 // without Pre Evaluation the non used directly single first mf val is inlined
      countSubstrings(code, "val valTry") shouldBe 0
      countSubstrings(code, "val valOption") shouldBe 0
      evaluated shouldBe Success(Some(6))
    }

    it should "pre evaluate only detected Monadic Flow Type" in new ToolboxTest {
      private val options = literalOptions(stringOptions :+ "Pre Evaluate Monadic Flow Type")

      val testCode: String =
        s"""$ToolboxTestImports
           |
           |mfcFor[Try[Option[Int]]]$options {
           |  val t1 = Try(1)
           |  Try(0) // to prevent single Try pre eval optimization
           |
           |  Some(0) + 0 // to prevent single Option pre eval optimization
           |  Some(t1 + 2) + 3
           |}
           |
           |""".stripMargin

      val (code, evaluated) = evaluatedCode(testCode)

      countSubstrings(code, "val t1") shouldBe 1
      countSubstrings(code, "val valTry") shouldBe 1
      countSubstrings(code, "val valOption") shouldBe 0
      evaluated shouldBe Success(Some(6))
    }

    it should "not pre evaluate detected Monadic Flow Type" in new ToolboxTest {
      private val options = literalOptions(stringOptions :+ "Pre Evaluate Option")

      val testCode: String =
        s"""$ToolboxTestImports
           |
           |mfcFor[Try[Option[Int]]]$options {
           |  val t1 = Try(1)
           |  Try(0) // to prevent single Try pre eval optimization
           |
           |  Some(0) + 0 // to prevent single Option pre eval optimization
           |  Some(t1 + 2) + 3
           |}
           |
           |""".stripMargin

      val (code, evaluated) = evaluatedCode(testCode)

      countSubstrings(code, "val t1") shouldBe 0 // without Pre Evaluation the non used directly single first mf val is inlined
      countSubstrings(code, "val valTry") shouldBe 0
      countSubstrings(code, "val valOption") shouldBe 2
      evaluated shouldBe Success(Some(6))
    }

    it should "pre evaluate all types in this Stack" in new ToolboxTest {
      private val options = literalOptions(stringOptions :+ "Pre Evaluate Option, Monadic Flow Type")

      val testCode: String =
        s"""$ToolboxTestImports
           |
           |mfcFor[Try[Option[Int]]]$options {
           |  val t1 = Try(1)
           |  Try(0) // to prevent single Try pre eval optimization
           |
           |  Some(0) + 0 // to prevent single Option pre eval optimization
           |  Some(t1 + 2) + 3
           |}
           |
           |""".stripMargin

      val (code, evaluated) = evaluatedCode(testCode)

      countSubstrings(code, "val t1") shouldBe 1
      countSubstrings(code, "val valTry") shouldBe 1
      countSubstrings(code, "val valOption") shouldBe 2
      evaluated shouldBe Success(Some(6))
    }

    it should "compile 'mfc' macro variant: NON-TOOLBOX" in {
      import sands.sugar.tms.TransparentMonads._

      mfc[Option[Int]] {
        val s = Some(0)
        s + 1
      } shouldBe Some(1)
    }

    it should "compile 'mfcFor' macro variant: NON-TOOLBOX" in {
      import sands.sugar.tms.TransparentMonads._

      mfcFor[Option[Int]]("ND") {
        val s = Some(0)
        s + 1
      } shouldBe Some(1)
    }

    it should "compile 'monadicFlowControl' macro variant: NON-TOOLBOX" in {
      import sands.sugar.tms.TransparentMonads._

      monadicFlowControl[Option[Int]] {
        val s = Some(0)
        s + 1
      } shouldBe Some(1)
    }

    it should "compile 'monadicFlowControlFor' macro variant: NON-TOOLBOX" in {
      import sands.sugar.tms.TransparentMonads._

      monadicFlowControlFor[Option[Int]]("ND") {
        val s = Some(0)
        s + 1
      } shouldBe Some(1)
    }

  }

  def monadicFlowTests(stringOptions: Seq[String]): Unit = {
    val options = literalOptions(stringOptions)

    val tmsOptions = TmsOptions.parseOptions(DefaultMfOptions, stringOptions, o => fail(s"invalid TmsOption is passed '$o'"))

    behavior of "tms Monadic Flow Behaviour with TmsOptions" + options

    it should "calculate mf block" in new ToolboxTest {
      val testCode: String =
        s"""$ToolboxTestImports
           |
           |val outer = Some(1)
           |
           |mfcFor[Option[Int]]$options {
           |  val dummy = Some(0)
           |  val inner = Some({
           |    val dummy = 1
           |    identity(dummy) // identity: to skip warning
           |    3
           |  })
           |  val incOuterWithInner = Some({
           |    val dummy = 2
           |    identity(dummy) // identity: to skip warning
           |    outer + 1 + inner
           |  })
           |
           |  // yield part
           |
           |  def double(x: Int) = {
           |    val dummy = 3
           |    identity(dummy) // identity: to skip warning
           |    2 * x
           |  }
           |
           |  double(incOuterWithInner + inner)
           |}
           |""".stripMargin

      evaluateCode(testCode) shouldBe Some(16)
    }

    it should "process yield of the inner monadic type not used in built Fors Stack" in new ToolboxTest {
      val testCode: String =
        s"""$ToolboxTestImports
           |
           |mfcFor[Option[Try[Int]]]$options {
           |  val i = Some(1)
           |
           |  Try[Int](i)
           |}
           |
           |""".stripMargin

      val (code, evaluated) = evaluatedCode(testCode)

      code should not include("flatMappedValueOfSome$")
      evaluated shouldBe Some(Try(1))
    }

    // flatMapping of the yield expression

    it should "postprocess and flatMap yield of the inner monadic type not used in built Fors Stack" in new ToolboxTest {
      val testCode: String =
        s"""$ToolboxTestImports
           |
           |mfcFor[Option[Try[Int]]]$options {
           |  val i = Some(1)
           |
           |  Some(Try[Int](i))
           |}
           |
           |""".stripMargin

      val (code, evaluated) = evaluatedCode(testCode)

      code should include("flatMappedValueOfSome$")
      evaluated shouldBe Some(Try(1))
    }

    it should "flatMap last Monadic Flow Type expression while postprocessing with reusing ready enum value of Monadic Flow val" in new ToolboxTest {
      val testCode: String =
        s"""$ToolboxTestImports
           |
           |mfcFor[Option[Int]]$options {
           |  Some(0)
           |  val i = Some(1)
           |  i
           |}
           |
           |""".stripMargin

      val (code, evaluated) = evaluatedCode(testCode)

      countSubstrings(code, "flatMappedValueOfSome") shouldBe 0
      countSubstrings(code, "iValue") shouldBe 2
      evaluated shouldBe Some(1)
    }

    it should "flatMap last Monadic Flow Type expression while postprocessing with creating flatMappedValueOf enum" in new ToolboxTest {
      val testCode: String =
        s"""$ToolboxTestImports
           |
           |mfcFor[Option[Int]]$options {
           |  Some(1)
           |  Some(2)
           |}
           |
           |""".stripMargin

      val (code, evaluated) = evaluatedCode(testCode)

      countSubstrings(code, "flatMappedValueOfSome") shouldBe 2
      evaluated shouldBe Some(2)
    }

    it should "not flatMap last Monadic Flow Type expression when there is second inner Monadic Flow Type in the Types Stack" in new ToolboxTest {
      val testCode: String =
        s"""$ToolboxTestImports
           |
           |mfcFor[Option[Option[Int]]]$options {
           |  Some(1)
           |  Some(2)
           |}
           |
           |""".stripMargin

      val (code, evaluated) = evaluatedCode(testCode)

      countSubstrings(code, "flatMappedValueOfSome") shouldBe 0
      evaluated shouldBe Some(Some(2))
    }

    // vals usage scope and accessibility

    it should "use mf val as initializer of other mf val (renaming)" in new ToolboxTest {
      val testCode: String =
        s"""$ToolboxTestImports
           |
           |mfcFor[Option[Int]]$options {
           |  Some(0) // mf type: i will be non first mf
           |  val i = Some(1)
           |  val j = i
           |
           |  i + j
           |}
           |""".stripMargin

      evaluateCode(testCode) shouldBe Some(2)
    }

    it should "use mf val directly in initializer of other mf val" in new ToolboxTest {
      val testCode: String =
        s"""$ToolboxTestImports
           |
           |mfcFor[Option[Int]]$options {
           |  Some(0) // mf type: i will be non first mf
           |  val i = Some(1)
           |  val j = i.orElse(None)
           |
           |  i + j
           |}
           |""".stripMargin

      evaluateCode(testCode) shouldBe Some(2)
    }

    it should "use mf val directly in initializer of non-mf val" in new ToolboxTest {
      val testCode: String =
        s"""$ToolboxTestImports
           |
           |mfcFor[Option[Int]]$options {
           |  Some(0) // mf type: i will be non first mf
           |  val i = Some(1)
           |  val j = i.isEmpty
           |  Some(2)
           |
           |  i + (if (j) 0 else i.get)
           |}
           |""".stripMargin

      evaluateCode(testCode) shouldBe Some(2)
    }

    it should "use mf val in tts in initializer of non-mf val" in new ToolboxTest {
      val testCode: String =
        s"""$ToolboxTestImports
           |
           |mfcFor[Option[Int]]$options {
           |  Some(0) // mf type: i will be non first mf
           |  val i = Some(1)
           |  val j = i + 2
           |
           |  i + j
           |}
           |""".stripMargin

      evaluateCode(testCode) shouldBe Some(4)
    }

    it should "use mf val as tts parameter in initializer of other mf val" in new ToolboxTest {
      val testCode: String =
        s"""$ToolboxTestImports
           |
           |mfcFor[Option[Int]]$options {
           |  Some(0) // mf type: i will be non first mf
           |  val i = Some(1)
           |  val j = Some(identity[Int](i))
           |
           |  i + j
           |}
           |""".stripMargin

      evaluateCode(testCode) shouldBe Some(2)
    }

    it should "use mf val as tts parameter in initializer of non-mf val" in new ToolboxTest {
      val testCode: String =
        s"""$ToolboxTestImports
           |
           |mfcFor[Option[Int]]$options {
           |  Some(0) // mf type: i will be non first mf
           |  val i = Some(1)
           |  val j = identity[Int](i)
           |
           |  i + j
           |}
           |""".stripMargin

      evaluateCode(testCode) shouldBe Some(2)
    }

    it should "use mf val directly and as tts parameter in yield" in new ToolboxTest {
      val testCode: String =
        s"""$ToolboxTestImports
           |
           |mfcFor[Option[Int]]$options {
           |  Some(0) // mf type: i will be non first mf
           |  val i = Some(1)
           |
           |  i.orElse(None) + i
           |}
           |""".stripMargin

      evaluateCode(testCode) shouldBe Some(2)
    }

    it should "use mf val in the initializer of the mf val that is built in the inner Fors Stack" in new ToolboxTest {
      val testCode: String =
        s"""$ToolboxTestImports
           |
           |mfcFor[Option[Int]]$options {
           |  val i = Some(1) // mf type
           |  val j = Some(2 + (i + 3)) // mf type
           |
           |  i + j
           |}
           |""".stripMargin

      evaluateCode(testCode) shouldBe Some(7)
    }

    it should "use non-mf val directly in initializer of mf val" in new ToolboxTest {
      val testCode: String =
        s"""$ToolboxTestImports
           |
           |mfcFor[Option[Int]]$options {
           |  Some(0) // mf type: i will be non first mf
           |  val i = 1
           |  val j = Some(i)
           |
           |  i + j
           |}
           |""".stripMargin

      evaluateCode(testCode) shouldBe Some(2)
    }

    it should "use non-mf val directly in initializer of non-mf val (renaming)" in new ToolboxTest {
      val testCode: String =
        s"""$ToolboxTestImports
           |
           |mfcFor[Option[Int]]$options {
           |  Some(0) // mf type: i will be non first mf
           |  val i = 1
           |  val k = i
           |  val j = Some(k)
           |
           |  i + j
           |}
           |""".stripMargin

      evaluateCode(testCode) shouldBe Some(2)
    }

    it should "use non-mf val in yield tts and directly" in new ToolboxTest {
      val testCode: String =
        s"""$ToolboxTestImports
           |
           |mfcFor[Option[Int]]$options {
           |  Some(0) // mf type
           |  val i = 1 // non-mf type
           |  val j = Some(2) // mf type
           |
           |  Some(i) + j + i
           |}
           |""".stripMargin

      evaluateCode(testCode) shouldBe Some(4)
    }

    it should "use non-mf val in the parameter of the mf type that is built in the inner Fors Stack" in new ToolboxTest {
      val testCode: String =
        s"""$ToolboxTestImports
           |
           |mfcFor[Option[Int]]$options {
           |  Some(0) // mf type
           |  val i = 1 // non-mf type
           |  val j = Some(2 + (i + Some(3))) // mf type
           |
           |  i + j
           |}
           |""".stripMargin

      evaluateCode(testCode) shouldBe Some(7)
    }

    it should "use last defined non-mf val in the tts of yield" in new ToolboxTest {
      val testCode: String =
        s"""$ToolboxTestImports
           |
           |mfcFor[Option[Int]]$options {
           |  Some(0) // mf type
           |  val i = 1 // non-mf type
           |
           |  Some(i) + 2
           |}
           |""".stripMargin

      evaluateCode(testCode) shouldBe Some(3)
    }

    it should "initialize mf val with the block having inner val with the same name" in new ToolboxTest {
      val testCode: String =
        s"""$ToolboxTestImports
           |
           |mfcFor[Option[Int]]$options {
           |  Some(0) // mf type: i will be non first mf
           |  val i = {
           |    val i = 1
           |    Some(i)
           |  }
           |
           |  i + 2
           |}
           |""".stripMargin

      evaluateCode(testCode) shouldBe Some(3)
    }

    // non Monadic Flow types of Types Stack usage (outer & inner than monadic flow type)

    it should "fail to use inner than mf type tts in the for enum val" in new ToolboxTest {
      val testCode: String =
        s"""$ToolboxTestImports
           |
           |mfcFor[Option[Try[Int]]]$options {
           |  val i = Some(1) // mf type
           |  val test: Int = i + Try(2) // inner stack type usage in the for enum val
           |  Some(0) // mf type
           |
           |  test + i
           |}
           |""".stripMargin

      Try(evaluateCode(testCode)) should matchPattern {
        case Failure(ToolBoxError(msg, _)) if msg.startsWith(
          "reflective typecheck has failed: extraction of the expression has consumed the part of Types Stack") =>
      }
    }

    it should "fail to use inner than mf type tts in the for enum expression" in new ToolboxTest {
      val testCode: String =
        s"""$ToolboxTestImports
           |
           |mfcFor[Option[Try[Int]]]$options {
           |  val i = Some(1) // mf type
           |  i + Try(2) // inner stack type usage in the for enum expr
           |  Some(0) // mf type
           |
           |  i + 3
           |}
           |""".stripMargin

      Try(evaluateCode(testCode)) should matchPattern {
        case Failure(ToolBoxError(msg, _)) if msg.startsWith(
          "reflective typecheck has failed: extraction of the expression has consumed the part of Types Stack") =>
      }
    }

    it should "use inner than mf type tts in the yield" in new ToolboxTest {
      val testCode: String =
        s"""$ToolboxTestImports
           |
           |mfcFor[Option[Try[Int]]]$options {
           |  val i = Some(1) // mf type
           |
           |  i + Try(2) // inner stack type usage in the yield
           |}
           |""".stripMargin

      evaluateCode(testCode) shouldBe Some(Success(3))
    }

    it should "use outer than mf type tts in the for enum val" in new ToolboxTest {
      val testCode: String =
        s"""$ToolboxTestImports
           |
           |mfcFor[Try[Option[Int]]]$options {
           |  val i = Some(1) // mf type
           |  val test: Int = i + Try(2) // outer stack type usage in the for enum val
           |  Some(0)
           |
           |  test + i
           |}
           |""".stripMargin

      evaluateCode(testCode) shouldBe Success(Some(4))
    }

    it should "use outer than mf type tts in the for enum expression" in new ToolboxTest {
      val testCode: String =
        s"""$ToolboxTestImports
           |
           |mfcFor[Try[Option[Int]]]$options {
           |  val i = Some(1) // mf type
           |  i + Try(2) // outer stack type usage in the for enum expr
           |  Some(0)
           |
           |  i + 3
           |}
           |""".stripMargin

      evaluateCode(testCode) shouldBe Success(Some(4))
    }

    it should "use outer than mf type tts in the yield" in new ToolboxTest {
      val testCode: String =
        s"""$ToolboxTestImports
           |
           |mfcFor[Try[Option[Int]]]$options {
           |  val i = Some(1) // mf type
           |
           |  i + Try(2) // outer stack type usage in the yield
           |}
           |""".stripMargin

      evaluateCode(testCode) shouldBe Success(Some(3))
    }

    it should "use the outer same stack type first" in new ToolboxTest {
      val testCode: String =
        s"""$ToolboxTestImports
           |
           |mfcFor[Try[Option[Try[Int]]]]$options {
           |  val i = Some(1) // mf type
           |  val test: Int = i + Try(2) // outer stack type usage, non-mf type
           |  Some(0)
           |
           |  Try(test + 1) // inner stack type usage
           |}
           |""".stripMargin

      evaluateCode(testCode) shouldBe Success(Some(Success(4)))
    }

    it should "fail to use yield val inside tts to be opened in Stack (out of its definition)" in new ToolboxTest {
      val testCode: String =
        s"""$ToolboxTestImports
           |
           |mfcFor[Try[Option[Try[Int]]]]$options {
           |  val i = Some(1) // mf type
           |  // yield
           |  val test: Int = i + Try(2) // outer stack type usage, non-mf type
           |  Try(test + 1) + 3 // inner stack type usage
           |}
           |""".stripMargin

      Try(evaluateCode(testCode)) should matchPattern {
        case Failure(ToolBoxError(msg, _)) if msg.startsWith(
          "reflective typecheck has failed: locally defined (in tmsCode) 'value test' may not be used") =>
      }
    }

    it should "use yield val inside tts with usage of inner tmsFor macro" in new ToolboxTest {
      val testCode: String =
        s"""$ToolboxTestImports
           |
           |mfcFor[Try[Option[Try[Int]]]]$options {
           |  val i = Some(1) // mf type
           |  // yield
           |  val test: Int = i + Try(2) // outer stack type usage, non-mf type
           |  tmsFor[Try[Int]]$options(Try(test + 1) + 3) // inner stack type usage by resulting Try[Int]
           |}
           |""".stripMargin

      evaluateCode(testCode) shouldBe Success(Some(Success(7)))
    }

    it should "fail to use mf val in the initializer of the mf val that is built in the inner Fors Stack with the outer non-mf type (when inner Fors Stacks are enabled)" in new ToolboxTest {
      val testCode: String =
        s"""$ToolboxTestImports
           |
           |mfcFor[Try[Option[Int]]]$options {
           |  val i = Some(1) // mf type
           |  val j = Some(Try(2) + 3 + (i + 4)) // mf type
           |
           |  i + j
           |}
           |""".stripMargin

      if (tmsOptions.forsStackForApplyParameter)
        Try(evaluateCode(testCode)) should matchPattern {
          case Failure(ToolBoxError(msg, _)) if msg.startsWith(
            "reflective typecheck has failed: locally defined (in tmsCode) 'value i' may not be used") =>
        }
      else
        evaluateCode(testCode) shouldBe Try(Some(11))
    }

    it should "fail to use mf val in tts with outer stack type than Monadic Flow Type (out of its definition scope)" in new ToolboxTest {
      val testCode: String =
        s"""$ToolboxTestImports
           |
           |mfcFor[Try[Option[Int]]]$options {
           |  val i = Some(1) // mf type
           |  // yield
           |  val test: Int = Try(i) // outer than mf type usage
           |  test + 1
           |}
           |""".stripMargin

      Try(evaluateCode(testCode)) should matchPattern {
        case Failure(ToolBoxError(msg, _)) if msg.startsWith(
          "reflective typecheck has failed: locally defined (in tmsCode) 'value i' may not be used") =>
      }
    }

    it should "fail to use mf val in tts2 with inner stack type than Monadic Flow Type" in new ToolboxTest {
      val testCode: String =
        s"""$ToolboxTestImports
           |
           |mfcFor[Option[Try[Int]]]$options {
           |  val i = Some(1) // mf type
           |  // yield
           |  val test: Int = Try(i) // inner than mf type usage of Try in tts2[Try, Option, Int](Try[Option[Int]](i))
           |  test + 1
           |}
           |""".stripMargin

      Try(evaluateCode(testCode)) should matchPattern {
        case Failure(ToolBoxError(msg, _)) if msg.startsWith(
          "reflective typecheck has failed: type Some could not be found in the non-reserved rest of the types stack []. Reserved types (from outer to the last used inner one inclusive) are [Option, Try]") =>
      }
    }

    it should "fail to use mf val in tts2 with outer stack type than Monadic Flow Type" in new ToolboxTest {
      val testCode: String =
        s"""$ToolboxTestImports
           |
           |mfcFor[Try[Option[Int]]]$options {
           |  val i = Some(1) // mf type
           |  // yield
           |  val test: Int = Try(i) // outer than mf type usage of Try in tts2[Try, Option, Int](Try[Option[Int]](i))
           |  test + 1
           |}
           |""".stripMargin

      Try(evaluateCode(testCode)) should matchPattern {
        case Failure(ToolBoxError(msg, _)) if msg.startsWith(
          "reflective typecheck has failed: locally defined (in tmsCode) 'value i' may not be used") =>
      }
    }

    it should "fail to use mf val in tts1 with inner stack type than Monadic Flow Type with ascription (val test goes to enum)" in new ToolboxTest {
      val testCode: String =
        s"""$ToolboxTestImports
           |
           |mfcFor[Option[Try[Int]]]$options {
           |  val i = Some(1) // mf type
           |  // yield
           |  val test: Int = Try(i: Int) // inner than mf type usage of Try in tts1[Try, Int]( Try[Int]( tts1[Option, Int]( i ) : Int )
           |  test + 1
           |}
           |""".stripMargin

      Try(evaluateCode(testCode)) should matchPattern {
        case Failure(ToolBoxError(msg, _)) if msg.startsWith(
          "reflective typecheck has failed: extraction of the expression has consumed the part of Types Stack [Option, Try] including type(s) inner than Monadic Flow Type Option") =>
      }
    }

    it should "use mf val in tts1 with inner stack type than Monadic Flow Type with ascription wrapped to block yield" in new ToolboxTest {
      val testCode: String =
        s"""$ToolboxTestImports
           |
           |mfcFor[Option[Try[Int]]]$options {
           |  val i = Some(1) // mf type
           |
           |  // yield
           |  {
           |    val test: Int = Try(i: Int) // inner than mf type usage of Try in tts1[Try, Int]( Try[Int]( tts1[Option, Int]( i ) : Int )
           |    test + 1 // inner than mf type usage of Try
           |  }
           |}
           |""".stripMargin

      evaluateCode(testCode) shouldBe Some(Try(2))
    }

    // pre evaluations

    it should "pre evaluate first mf type val of the first block statement always" in new ToolboxTest {
      val testCode: String =
        s"""$ToolboxTestImports
           |
           |mfcFor[Option[Int]]$options {
           |  val i = Some(1) // first mf type val in the first block statement
           |
           |  i.get // i is used directly without tts so this blocks i pre evaluation val optimization
           |  i + 2
           |}
           |""".stripMargin

      val (code, evaluated) = evaluatedCode(testCode)

      countSubstrings(code, """\{\s*val i.+\s*i\.map""") shouldBe 1

      evaluated shouldBe Some(3)
    }

    it should "pre evaluate first mf type val of the non-first block statement when Pre Evaluation is On" in new ToolboxTest {
      val testCode: String =
        s"""$ToolboxTestImports
           |
           |mfcFor[Option[Int]]$options {
           |  Some(0)
           |  val i = Some(1) // first mf type val in the first block statement
           |
           |  i + 2
           |}
           |""".stripMargin

      val (code, evaluated) = evaluatedCode(testCode)

      countSubstrings(code, """val i.+\s*valOption.+\.flatMap""") shouldBe (if (tmsOptions.preEvaluateTypes.isEmpty) 0 else 1)

      evaluated shouldBe Some(3)
    }

    it should "pre evaluate all mf vals when one depends on another (even if just renaming - stable val is used directly)" in new ToolboxTest {
      val testCode: String =
        s"""$ToolboxTestImports
           |
           |mfcFor[Option[Int]]$options {
           |  val i = Some(1)
           |  val j = i
           |  val k = j.orElse(None)
           |
           |  i + j + k
           |}
           |""".stripMargin

      val (code, evaluated) = evaluatedCode(testCode)

      countSubstrings(code, """\{\s*val i.+\s*val j.+\s*val k""") shouldBe (if (tmsOptions.preEvaluateTypes.isEmpty) 0 else 1)

      evaluated shouldBe Some(3)
    }

    it should "skip pre evaluation of mf val when its initializer depends on mf type expression" in new ToolboxTest {
      val testCode: String =
        s"""$ToolboxTestImports
           |
           |mfcFor[Option[Int]]$options {
           |  val i = Some(1) // will be pre evaluated always (first Enum mf val with direct usage in yield)
           |  val j = Some[Int](Some(2)) // skips pre evaluation: j depends on extraction of `tts1[Some, Int](Some(2))` (that is also pre evaluated)
           |
           |  i.get + j
           |}
           |""".stripMargin

      val (code, evaluated) = evaluatedCode(testCode)

      countSubstrings(code, """\{\s*val i.+\s*val j""") shouldBe 0
      if (tmsOptions.preEvaluateTypes.isEmpty)
        countSubstrings(code, """\{\s*val i.+\s*i\.flatMap""") shouldBe 1
      else
        countSubstrings(code, """\{\s*val i.+\s*val valOption.+\s*i\.flatMap""") shouldBe 1

      evaluated shouldBe Some(3)
    }

    it should "pre evaluate mf type expressions when Pre Evaluation is On" in new ToolboxTest {
      val testCode: String =
        s"""$ToolboxTestImports
           |
           |mfcFor[Option[Int]]$options {
           |  Some(0)
           |  Some(1)
           |
           |  3
           |}
           |""".stripMargin

      val (code, evaluated) = evaluatedCode(testCode)

      countSubstrings(code, """val valOption.+\s*val valOption.+\s*valOption.+\.flatMap""") shouldBe (if (tmsOptions.preEvaluateTypes.isEmpty) 0 else 1)

      evaluated shouldBe Some(3)
    }

    it should "skip pre evaluation of mf type expressions when it depends on mf type expression" in new ToolboxTest {
      val testCode: String =
        s"""$ToolboxTestImports
           |
           |mfcFor[Option[Int]]$options {
           |  Some(0) // to block single pre eval optimization while postprocessing
           |  Some[Int](Some(1)) // skips pre evaluation: it depends on extraction of `tts1[Some, Int](Some(1))` (that is pre evaluated itself)
           |
           |  3
           |}
           |""".stripMargin

      val (code, evaluated) = evaluatedCode(testCode)

      countSubstrings(code, """\{\s*val valOption.+\s*val valOption.+\s*valOption.+\.flatMap""") shouldBe (if (tmsOptions.preEvaluateTypes.isEmpty) 0 else 1)

      evaluated shouldBe Some(3)
    }

    it should "skip pre evaluation of mf type expressions when it depends on mf type val's tts and should inline first mf val" in new ToolboxTest {
      val testCode: String =
        s"""$ToolboxTestImports
           |
           |mfcFor[Option[Int]]$options {
           |  val i = Some(1)
           |  Some[Int](i) // skips pre evaluation: it depends on extraction of `tts1[Some, Int](i)`
           |
           |  3
           |}
           |""".stripMargin

      val (code, evaluated) = evaluatedCode(testCode)

      countVals(code) shouldBe 0

      evaluated shouldBe Some(3)
    }

    it should "skip pre evaluation of mf type expressions when it is exactly mf type val" in new ToolboxTest {
      val testCode: String =
        s"""$ToolboxTestImports
           |
           |mfcFor[Option[Int]]$options {
           |  val i = Some(1)
           |  i // skips pre evaluation, reasonably warns: pure expression does nothing
           |
           |  i + 2
           |}
           |""".stripMargin

      val (code, evaluated) = evaluatedCode(testCode)

      countSubstrings(code, """\{\s*val i.+\s*i\.flatMap""") shouldBe 1

      evaluated shouldBe Some(3)
    }

    it should "skip pre evaluation of mf type expressions when it is exactly stable non-lazy ident" in new ToolboxTest {
      val testCode: String =
        s"""$ToolboxTestImports
           |
           |val stableNonLazy = Some(1)
           |
           |mfcFor[Option[Int]]$options {
           |  val i = Some(2)
           |  stableNonLazy // skips pre evaluation, reasonably warns: pure expression does nothing
           |
           |  i.get + stableNonLazy // .get is to prevent inlining of the mf val while postprocessing
           |}
           |""".stripMargin

      val (code, evaluated) = evaluatedCode(testCode)

      countSubstrings(code, """\{\s*val i.+\s*i\.flatMap""") shouldBe 1

      evaluated shouldBe Some(3)
    }

    it should "pre evaluate mf type enum val even if its initializer contains local val with the same name as preceding non-mf type val" in new ToolboxTest {
      val testCode: String =
        s"""$ToolboxTestImports
           |
           |mfcFor[Option[Int]]$options {
           |  Some(0) // mf type
           |  val i = 1
           |  val sameNameInside = Some({
           |    val i = 2
           |    i
           |  })
           |
           |  i + sameNameInside.get // .get is to prevent inlining of the mf val while postprocessing
           |}
           |""".stripMargin

      val (code, evaluated) = evaluatedCode(testCode)

      countSubstrings(code, """val sameNameInside""") shouldBe 1
      countSubstrings(code, """map.+\s*.+\s*val sameNameInside""") shouldBe (if (tmsOptions.preEvaluateTypes.isEmpty) 1 else 0)

      evaluated shouldBe Some(3)
    }

    it should "pre evaluate mf type enum val even if its initializer contains local val with the same name as following mf type val" in new ToolboxTest {
      val testCode: String =
        s"""$ToolboxTestImports
           |
           |mfcFor[Option[Int]]$options {
           |  val sameNameInside = Some({
           |    val i = 2
           |    i
           |  })
           |  val i = Some(1)
           |
           |  i + sameNameInside.get // .get is to prevent inlining of the mf val while postprocessing
           |}
           |""".stripMargin

      val (code, evaluated) = evaluatedCode(testCode)

      countSubstrings(code, """\{\s*  val sameNameInside""") shouldBe 1

      evaluated shouldBe Some(3)
    }

    // inlining not used mf vals while postprocessing

    it should "postprocess not used directly single mf val and inline its pre eval" in new ToolboxTest {
      val testCode: String =
        s"""$ToolboxTestImports
           |
           |mfcFor[Option[Int]]$options {
           |  val mf1 = Some(1) // mf type
           |
           |  mf1 + 2
           |}
           |
           |""".stripMargin

      val (code, evaluated) = evaluatedCode(testCode)

      countVals(code) shouldBe 0

      evaluated shouldBe Some(3)
    }

    it should "postprocess used directly (in the yield) single mf val and do not inline its pre eval" in new ToolboxTest {
      val testCode: String =
        s"""$ToolboxTestImports
           |
           |mfcFor[Option[Int]]$options {
           |  val mf1 = Some(1) // mf type
           |
           |  mf1.get + 2
           |}
           |
           |""".stripMargin

      val (code, evaluated) = evaluatedCode(testCode)

      countVals(code) shouldBe 1
      countSubstrings(code, """\{\s*val mf1 .+\s*mf1\.map""") shouldBe 1

      evaluated shouldBe Some(3)
    }

    it should "postprocess used directly (in the for-enum) single mf val and do not inline its pre eval" in new ToolboxTest {
      val testCode: String =
        s"""$ToolboxTestImports
           |
           |mfcFor[Option[Int]]$options {
           |  val mf1 = Some(1) // mf type
           |  Some(mf1.get)
           |
           |  mf1 + 2
           |}
           |
           |""".stripMargin

      val (code, evaluated) = evaluatedCode(testCode)

      countVals(code) shouldBe (if (tmsOptions.preEvaluateTypes.isEmpty) 1 else 2) // 2: plus 'Some(mf1.get)' expression pre eval
      countSubstrings(code, """\{\s*val mf1""") shouldBe 1

      evaluated shouldBe Some(3)
    }

    it should "postprocess used directly (in the inner collector) single mf val and do not inline its pre eval" in new ToolboxTest {
      val testCode: String =
        s"""$ToolboxTestImports
           |
           |mfcFor[Option[Try[Int]]]$options {
           |  val mf1 = Some(1) // mf type
           |
           |  identity[Int](Try(mf1.get))
           |  mf1 + 2
           |}
           |
           |""".stripMargin

      val (code, evaluated) = evaluatedCode(testCode)

      countVals(code) shouldBe 1
      countSubstrings(code, """\{\s*val mf1""") shouldBe 1

      evaluated shouldBe Some(Try(3))
    }

    it should "postprocess used directly (in the yield) enum val and do not inline its assign enum" in new ToolboxTest {
      val testCode: String =
        s"""$ToolboxTestImports
           |
           |mfcFor[Option[Int]]$options {
           |  val mf1 = Some[Int](Some(1)) // mf type, tts1(Some(1)) will be extracted in the 1st enum, so mf1 - also in enums '=' & '<-'
           |
           |  mf1.get + 2
           |}
           |
           |""".stripMargin

      val (code, evaluated) = evaluatedCode(testCode)

      countSubstrings(code, """\{\s*val mf1""") shouldBe 1

      evaluated shouldBe Some(3)
    }

    it should "postprocess used directly (in the for-enum) enum val and do not inline its assign enum" in new ToolboxTest {
      val testCode: String =
        s"""$ToolboxTestImports
           |
           |mfcFor[Option[Int]]$options {
           |  val mf1 = Some[Int](Some(1)) // mf type, tts1(Some(1)) will be extracted in the 1st enum, so mf1 - also in enums '=' & '<-'
           |  Some(mf1.get)
           |
           |  mf1 + 2
           |}
           |
           |""".stripMargin

      val (code, evaluated) = evaluatedCode(testCode)

      countSubstrings(code, """\{\s*val mf1""") shouldBe 1

      evaluated shouldBe Some(3)
    }

    it should "postprocess used directly (in the inner collector) enum val and do not inline its assign enum, inline both collectors preEvals" in new ToolboxTest {
      val testCode: String =
        s"""$ToolboxTestImports
           |
           |mfcFor[Option[Try[Int]]]$options {
           |  val mf1 = Some[Int](Some(1)) // mf type, tts1(Some(1)) will be extracted in the 1st enum, so mf1 - also in enums '=' & '<-'
           |
           |  identity[Int](Try(mf1.get))
           |  mf1 + 2
           |}
           |
           |""".stripMargin

      val (code, evaluated) = evaluatedCode(testCode)

      countSubstrings(code, """\{\s*val mf1""") shouldBe 1
      countVals(code) shouldBe 1

      evaluated shouldBe Some(Try(3))
    }

    it should "postprocess 2 not used directly mf vals and when pre evaluation is On: do not inline one's pre evals; when pre evaluation is Off: inline both pre eval & enum val" in new ToolboxTest {
      val testCode: String =
        s"""$ToolboxTestImports
           |
           |mfcFor[Option[Int]]$options {
           |  val mf1 = Some(1) // mf type
           |  val mf2 = Some(2) // mf type
           |
           |  mf1 + mf2
           |}
           |
           |""".stripMargin

      val (code, evaluated) = evaluatedCode(testCode)

      val twoPreEvalsRegex = """\{\s*val mf1.+\s*val mf2.+\s*mf1\.flatMap"""

      if (tmsOptions.preEvaluateTypes.isEmpty) { // preEval is Off for Option: single pre eval of first mf1 will be inlined and enum val mf2 is also
        countVals(code) shouldBe 0
        countSubstrings(code, twoPreEvalsRegex) shouldBe 0
      } else { // preEval is On for Option
        countVals(code) shouldBe 2
        countSubstrings(code, twoPreEvalsRegex) shouldBe 1
      }

      evaluated shouldBe Some(3)
    }

    it should "postprocess and inline mf vals depending on options: when pre evaluation is Off: do not inline vals used in the inner For Stack built in Fors Stack For Apply Source mode" in new ToolboxTest {
      val testCode: String =
        s"""$ToolboxTestImports
           |
           |mfcFor[Option[Int]]$options {
           |  val mf1 = Some(1) // mf type
           |  val mf2 = Some(2) // mf type
           |  val mf3 = Some(3) // mf type
           |
           |  (mf1 + mf2) + mf3
           |}
           |
           |""".stripMargin

      val (code, evaluated) = evaluatedCode(testCode)

      val threePreEvalsRegex = """\{\s*val mf1.+\s*val mf2.+\s*val mf3.+\s*mf1\.flatMap"""

      if (tmsOptions.preEvaluateTypes.isEmpty) { // preEval is Off for Option: single pre eval of first mf1 and both enum vals mf2 & mf3 will be inlined
        countVals(code) shouldBe (if (tmsOptions.forsStackForApplySource) 2 else 0) // in forsStackForApplySource mode: mf1 & mf2 are used in the Inner Fors Stack: do not inlined
        countSubstrings(code, threePreEvalsRegex) shouldBe 0
      } else { // preEval is On for Option
        countVals(code) shouldBe (if (tmsOptions.forsStackForApplySource) 4 else 3) // in forsStackForApplySource mode: 3 defined vals + val for Inner Fors Stack: nothing is inlined
        countSubstrings(code, threePreEvalsRegex) shouldBe (if (tmsOptions.forsStackForApplySource) 0 else 1) // in No forsStackForApplySource mode: 3 defined vals are pre evaluated and no one is inlined
      }

      evaluated shouldBe Some(6)
    }

    it should "postprocess and inline mf vals depending on options: when pre evaluation is Off: do not inline vals used in the inner For Stack built in Fors Stack For Apply Parameter mode" in new ToolboxTest {
      val testCode: String =
        s"""$ToolboxTestImports
           |
           |mfcFor[Option[Int]]$options {
           |  val mf1 = Some(1) // mf type
           |  val mf2 = Some(2) // mf type
           |  val mf3 = Some(3) // mf type
           |
           |  mf1 + (mf2 + mf3)
           |}
           |
           |""".stripMargin

      val (code, evaluated) = evaluatedCode(testCode)

      val threePreEvalsRegex = """\{\s*val mf1.+\s*val mf2.+\s*val mf3.+\s*mf1\.flatMap"""

      if (tmsOptions.preEvaluateTypes.isEmpty) { // preEval is Off for Option: single pre eval of first mf1 and both enum vals mf2 & mf3 will be inlined
        countVals(code) shouldBe (if (tmsOptions.forsStackForApplyParameter) 2 else 0) // in forsStackForApplyParameter mode: mf2 & mf3 are used in the Inner Fors Stack: do not inlined
        countSubstrings(code, threePreEvalsRegex) shouldBe 0
      } else { // preEval is On for Option
        countVals(code) shouldBe (if (tmsOptions.forsStackForApplyParameter) 4 else 3) // in forsStackForApplyParameter mode: 3 defined vals + val for Inner Fors Stack: nothing is inlined
        countSubstrings(code, threePreEvalsRegex) shouldBe (if (tmsOptions.forsStackForApplyParameter) 0 else 1) // in No forsStackForApplyParameter mode: 3 defined vals are pre evaluated and no one is inlined
      }

      evaluated shouldBe Some(6)
    }

    it should "postprocess 3 used directly mf vals and inline nothing in any mode" in new ToolboxTest {
      val testCode: String =
        s"""$ToolboxTestImports
           |
           |mfcFor[Option[Int]]$options {
           |  val mf1 = Some(1) // mf type
           |  val mf2 = Some(2) // mf type
           |  val mf3 = Some(3) // mf type
           |
           |  mf1.get + mf2.get + mf3.get
           |}
           |
           |""".stripMargin

      val (code, evaluated) = evaluatedCode(testCode)

      countVals(code) shouldBe 3

      evaluated shouldBe Some(6)
    }

  }
}
