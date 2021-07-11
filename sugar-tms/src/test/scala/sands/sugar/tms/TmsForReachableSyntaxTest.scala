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
import sands.sugar.tms.TransparentMonads.tmsOptions

/*
 * Created by Serhiy Shamshetdinov
 * at 18.02.2021 18:38
 */

@tmsOptions("No Debug", "No Trace") // for NON-TOOLBOX tests
class TmsForReachableSyntaxTest extends TmsTestBase {

  reachableSyntaxTests(SimpleForsStackOptions) // some tests reasonably fails with Inner Fors Stack options

  def reachableSyntaxTests(tmsOptions: Seq[String]): Unit = {
    val options = literalOptions(tmsOptions)

    behavior of "tms macros AST Reachable Syntax with TmsOptions" + options

    it should "access no args method of value of inner type without parentheses" in new ToolboxTest {
      val testCode: String =
        s"""$ToolboxTestImports
           |
           |val optionString = Option("Hi!")
           |
           |tmsFor[Option[Int]]$options(optionString.length)
           |
           |""".stripMargin

      evaluateCode(testCode) shouldBe Some(3)
    }

    it should "access no args method of value of inner type with parentheses" in new ToolboxTest {
      val testCode: String =
        s"""$ToolboxTestImports
           |
           |val optionString = Option("Hi!")
           |
           |tmsFor[Option[Int]]$options(optionString.length())
           |
           |""".stripMargin

      evaluateCode(testCode) shouldBe Some(3)
    }

    it should "access method of value of inner type with parameter" in new ToolboxTest {
      val testCode: String =
        s"""$ToolboxTestImports
           |
           |val optionInt = Some(1)
           |
           |tmsFor[Option[Int]]$options(optionInt + 1)
           |
           |""".stripMargin

      evaluateCode(testCode) shouldBe Some(2)
    }

    it should "pass value of inner type as parameter of a class method" in new ToolboxTest {
      val testCode: String =
        s"""$ToolboxTestImports
           |
           |val optionInt = Option(1)
           |
           |tmsFor[Option[Int]]$options(2 + optionInt)
           |
           |""".stripMargin

      evaluateCode(testCode) shouldBe Some(3)
    }

    it should "pass value of inner type as parameter of method without select" in new ToolboxTest {
      val testCode: String =
        s"""$ToolboxTestImports
           |
           |val optionInt = Option(1)
           |def a = {
           |  def identityInt(i: Int): Int = i
           |  tmsFor[Option[Int]]$options(identityInt(optionInt))
           |}
           |a
           |""".stripMargin

      evaluateCode(testCode) shouldBe Some(1)
    }

    it should "pass value of inner type as parameter of typed method" in new ToolboxTest {
      val testCode: String =
        s"""$ToolboxTestImports
           |
           |val optionInt = Option(1)
           |
           |tmsFor[Option[Int]]$options(identity[Int](optionInt))
           |
           |""".stripMargin

      evaluateCode(testCode) shouldBe Some(1)
    }

    it should "pass 2 values of inner type as parameters" in new ToolboxTest {
      val testCode: String =
        s"""$ToolboxTestImports
           |
           |def mul(arg1: Int, arg2: Int): Int = arg1 * arg2
           |
           |tmsFor[Option[Int]]$options(mul(Some(2), Some(3)))
           |
           |""".stripMargin

      evaluateCode(testCode) shouldBe Some(6)
    }

    it should "pass values of inner type to method with multi-list parameters" in new ToolboxTest {
      val testCode: String =
        s"""$ToolboxTestImports
           |
           |def mul(arg1: Int)(arg2: Int): Int = arg1 * arg2
           |
           |tmsFor[Option[Int]]$options(mul(Some(3))(Some(2)))
           |
           |""".stripMargin

      evaluateCode(testCode) shouldBe Some(6)
    }

    it should "pass 2 values of inner type as parameters to method of the optional instance" in new ToolboxTest {
      val testCode: String =
        s"""$ToolboxTestImports
           |class TestClass() {
           |  def mul(arg1: Int, arg2: Int): Int = arg1 * arg2
           |}
           |tmsFor[Option[Int]]$options(Some(new TestClass()).mul(Some(3), Some(4)))
           |
           |""".stripMargin

      evaluateCode(testCode) shouldBe Some(12)
    }

    it should "process ascription : Type" in new ToolboxTest {
      val testCode: String =
        s"""$ToolboxTestImports
           |
           |tmsFor[Option[Long]]$options(Some(1): Long)
           |
           |""".stripMargin

      evaluateCode(testCode) shouldBe Some(1)
    }

    it should "process annotated ascription : Type @unchecked" in new ToolboxTest {
      val testCode: String =
        s"""$ToolboxTestImports
           |
           |tmsFor[Option[Long]]$options(Some(1): Long @unchecked)
           |
           |""".stripMargin

      evaluateCode(testCode) shouldBe Some(1)
    }

    // 2 test does not work in toolbox (at least in 2.11) with the strange difference of the same output Tree (looks like a bug in the showCode or typecheck + un-typecheck): first is correct
    // [Debug] * tmsFor OUTPUT code.tree: scala.Some.apply[Int](1).map(((valueOfSome$macro$1) => 1.$plus(((twiceInt(valueOfSome$macro$1): Int @unchecked): Int @noinline @unchecked))))
    // [Debug] * tmsFor OUTPUT showCode(code.tree): scala.Some.apply[Int](1).map(((valueOfSome$macro$1) => (1).+(((twiceInt(sands.sugar.tms.TransparentMonads.tts1[Some, Int](scala.Some.apply[Int](1))): @unchecked): @noinline))))
    it should "process TmsApply with ascripted argument with the only 2 annotations : @unchecked @noinline: NON-TOOLBOX" in {
      {
        import sands.sugar.tms.TransparentMonads._

        def twiceInt(i: Int): Int = 2 * i

        tmsFor[Option[Long]]()(1 + (twiceInt(Some(1)): @unchecked @noinline))
      } shouldBe Some(3)
    }

    it should "process TmsApply with ascripted argument with the only 1 annotation : @noinline: NON-TOOLBOX" in {
      {
        import sands.sugar.tms.TransparentMonads._

        def twiceInt(i: Int): Int = 2 * i

        tmsFor[Option[Long]]()(1 + (twiceInt(Some(1)): @noinline))
      } shouldBe Some(3)
    }

    // 2 test does not work in toolbox (at least in 2.11) with the strange difference of the same output Tree (looks like a bug in the showCode or typecheck + un-typecheck): first is correct
    // [Debug] * tmsFor OUTPUT code.tree: scala.Some.apply[Int](1).map(((valueOfSome$macro$1) => ((twiceInt(valueOfSome$macro$1): Int @unchecked): Int @noinline @unchecked).$plus(1)))
    // [Debug] * tmsFor OUTPUT showCode(code.tree): scala.Some.apply[Int](1).map(((valueOfSome$macro$1) => ((twiceInt(sands.sugar.tms.TransparentMonads.tts1[Some, Int](scala.Some.apply[Int](1))): @unchecked): @noinline).+(1)))
    it should "process TmsApply with ascripted source with the only 2 annotations : @unchecked @noinline: NON-TOOLBOX" in {
      {
        import sands.sugar.tms.TransparentMonads._

        def twiceInt(i: Int): Int = 2 * i

        tmsFor[Option[Long]]()((twiceInt(Some(1)): @unchecked @noinline) + 1)
      } shouldBe Some(3)
    }

    it should "process TmsApply with ascripted source with the only 1 annotation : @noinline: NON-TOOLBOX" in {
      {
        import sands.sugar.tms.TransparentMonads._

        def twiceInt(i: Int): Int = 2 * i

        tmsFor[Option[Long]]()((twiceInt(Some(1)): @noinline) + 1)
      } shouldBe Some(3)
    }

    it should "process 'if' expression with tts1 predicate with 'else' clause" in new ToolboxTest {
      val testCode: String =
        s"""$ToolboxTestImports
           |
           |tmsFor[Option[Int]]$options( if (Some(false)) 1 else 2 )
           |
           |""".stripMargin

      evaluateCode(testCode) shouldBe Some(2)
    }

    it should "process 'if' expression with tts1 predicate without 'else' clause" in new ToolboxTest {
      val testCode: String =
        s"""$ToolboxTestImports
           |
           |var v = 0 // to hide warning: a pure expression does nothing in statement position
           |
           |tmsFor[Option[Unit]]$options( if (Some(true)) {v = 1} )
           |
           |""".stripMargin

      evaluateCode(testCode) shouldBe Some(())
    }

    it should "process 'if' expression with tts1 'then' clause" in new ToolboxTest {
      val testCode: String =
        s"""$ToolboxTestImports
           |
           |tmsFor[Option[Int]]$options( if (true) Some(1) + 2 else 1 )
           |
           |""".stripMargin

      evaluateCode(testCode) shouldBe Some(3)
    }

    it should "process 'if' expression with tts1 'else' clause" in new ToolboxTest {
      val testCode: String =
        s"""$ToolboxTestImports
           |
           |tmsFor[Option[Int]]$options( if (false) 1 else Some(1) + 3 )
           |
           |""".stripMargin

      evaluateCode(testCode) shouldBe Some(4)
    }

    it should "process 'if' expression with tts1 predicate & clauses" in new ToolboxTest {
      val testCode: String =
        s"""$ToolboxTestImports
           |
           |tmsFor[Option[Int]]$options( if (Some(true)) Some(1) + 1 else Some(2) + 1 )
           |
           |""".stripMargin

      evaluateCode(testCode) shouldBe Some(2)
    }

    it should "flatMap 'if' expression with tts1 predicate & non-tts clauses of the same monadic type" in new ToolboxTest {
      val testCode: String =
        s"""$ToolboxTestImports
           |
           |tmsFor[Option[Int]]$options( if (Some(true)) Some(1) else Some(2) )
           |
           |""".stripMargin

      evaluateCode(testCode) shouldBe Some(1)
    }

    it should "flatMap 'if' expression with tts1 predicate & non-tts clauses of the same monadic type with val" in new ToolboxTest {
      val testCode: String =
        s"""$ToolboxTestImports
           |
           |val someTrue = Some(true)
           |tmsFor[Option[Boolean]]$options( if (someTrue) someTrue else someTrue )
           |
           |""".stripMargin

      evaluateCode(testCode) shouldBe Some(true)
    }

    it should "process 'while' statement with tts1 predicate" in new ToolboxTest {
      val testCode: String =
        s"""$ToolboxTestImports
           |
           |var v = 0 // to hide warning: a pure expression does nothing in statement position
           |
           |tmsFor[Option[Unit]]$options( while (Some(false) & true) {v = 1} )
           |
           |""".stripMargin

      evaluateCode(testCode) shouldBe Some(())
    }

    it should "process 'while' statement with tts1 body" in new ToolboxTest {
      val testCode: String =
        s"""$ToolboxTestImports
           |
           |tmsFor[Option[Unit]]$options( while (false) {Some(1) + 1} )
           |
           |""".stripMargin

      evaluateCode(testCode) shouldBe Some(())
    }

    it should "process 'while' statement with both tts1 predicate & body" in new ToolboxTest {
      val testCode: String =
        s"""$ToolboxTestImports
           |
           |tmsFor[Option[Unit]]$options( while (Some(false) & true) {Some(1) + 1} )
           |
           |""".stripMargin

      evaluateCode(testCode) shouldBe Some(())
    }

    it should "process 'do-while' statement with tts1 predicate" in new ToolboxTest {
      val testCode: String =
        s"""$ToolboxTestImports
           |
           |var v = 0 // to hide warning: a pure expression does nothing in statement position
           |
           |tmsFor[Option[Unit]]$options( do {v = 1} while (Some(false) & true) )
           |
           |""".stripMargin

      evaluateCode(testCode) shouldBe Some(())
    }

    it should "process 'do-while' statement with tts1 body" in new ToolboxTest {
      val testCode: String =
        s"""$ToolboxTestImports
           |
           |tmsFor[Option[Unit]]$options( do {Some(1) + 1} while (false) )
           |
           |""".stripMargin

      evaluateCode(testCode) shouldBe Some(())
    }

    it should "process 'do-while' statement with both tts1 predicate & body" in new ToolboxTest {
      val testCode: String =
        s"""$ToolboxTestImports
           |
           |tmsFor[Option[Unit]]$options( do {Some(1) + 1} while (Some(false) & true) )
           |
           |""".stripMargin

      evaluateCode(testCode) shouldBe Some(())
    }

    it should "process Tuples sugar syntax" in new ToolboxTest {
      val testCode: String =
        s"""$ToolboxTestImports
           |
           |tmsFor[Option[Int]]$options( Some((1, 2, 3))._1 + Some((1, 2, 3, 4, 5, 6))._6 )
           |
           |""".stripMargin

      evaluateCode(testCode) shouldBe Some(7)
    }

    it should "process 'return' expression with tts1" in new ToolboxTest {
      val testCode: String =
        s"""$ToolboxTestImports
           |
           |def test(): Int = {
           |  tmsFor[Try[Int]]$options( return Try(1) + 2 )
           |  0
           |}
           |
           |test()
           |
           |""".stripMargin

      evaluateCode(testCode) shouldBe 3
    }

    it should "process block of 2 expressions each with tts1" in new ToolboxTest {
      val testCode: String =
        s"""$ToolboxTestImports
           |
           |tmsFor[Option[Int]]$options {
           |  1 - (Some(0) + 1)
           |  1 + (Some(0) + 1)
           |}
           |
           |""".stripMargin

      evaluateCode(testCode) shouldBe Some(2)
    }

    it should "process block with tts as source of apply" in new ToolboxTest {
      val testCode: String =
        s"""$ToolboxTestImports
           |
           |tmsFor[Option[Int]]$options {
           |  {
           |    val i = 5
           |    Some(1) - 1
           |    i
           |  } + Some(1)
           |}
           |
           |""".stripMargin

      evaluateCode(testCode) shouldBe Some(6)
    }

    it should "process block with tts as argument" in new ToolboxTest {
      val testCode: String =
        s"""$ToolboxTestImports
           |
           |tmsFor[Option[Int]]$options {
           |  identity[Int]({
           |    val i = 5
           |    Some(1) - 1
           |    i
           |  }) + Some(1)
           |}
           |
           |""".stripMargin

      evaluateCode(testCode) shouldBe Some(6)
    }

    it should "process block with tts as argument of tts" in new ToolboxTest {
      val testCode: String =
        s"""$ToolboxTestImports
           |
           |tmsFor[Option[Int]]$options {
           |  Some({
           |    val i = 5
           |    Some(1) - 1
           |    i
           |  }) + 1
           |}
           |
           |""".stripMargin

      evaluateCode(testCode) shouldBe Some(6)
    }

    it should "process block with tts1 as parameter" in new ToolboxTest {
      val testCode: String =
        s"""$ToolboxTestImports
           |
           |tmsFor[Option[Int]]$options {
           |  1 + {
           |    val i = 5
           |    Some(1) - i
           |  }
           |}
           |
           |""".stripMargin

      evaluateCode(testCode) shouldBe Some(-3)
    }

    it should "process var assignment with tts" in new ToolboxTest {
      val testCode: String =
        s"""$ToolboxTestImports
           |
           |tmsFor[Option[Int]]$options {
           |  var a = 6
           |  a += Some(3)
           |  a
           |}
           |
           |""".stripMargin

      evaluateCode(testCode) shouldBe Some(9)
    }

    it should "process for-yield with tts in argument and yield body" in new ToolboxTest {
      val testCode: String =
        s"""$ToolboxTestImports
           |
           |tmsFor[Option[Int]]$options {
           |  for {
           |    ov <- Some(Some(3) + 1)
           |  } yield
           |    ov + Some(2)
           |}
           |
           |""".stripMargin

      evaluateCode(testCode) shouldBe Some(6)
    }

    it should "process for-each with tts in argument and body" in new ToolboxTest {
      val testCode: String =
        s"""$ToolboxTestImports
           |
           |tmsFor[Option[Int]]$options {
           |  var s = 0
           |  for {
           |    ov <- Some(Some(3) + 1)
           |  } {
           |    s = ov + Some(2)
           |  }
           |  s
           |}
           |
           |""".stripMargin

      evaluateCode(testCode) shouldBe Some(6)
    }

    it should "access function as tts parameter" in new ToolboxTest {
      val testCode: String =
        s"""$ToolboxTestImports
           |
           |tmsFor[Option[Int]]$options(Some((i: Int, j: Int) => i + j)(1, 2))
           |
           |""".stripMargin

      evaluateCode(testCode) shouldBe Some(3)
    }

    it should "access function with placeholders as tts parameter" in new ToolboxTest {
      val testCode: String =
        s"""$ToolboxTestImports
           |
           |tmsFor[Option[Int]]$options(Some((_: Int) + (_: Int))(1, 2))
           |
           |""".stripMargin

      evaluateCode(testCode) shouldBe Some(3)
    }

    it should "process tts in function body" in new ToolboxTest {
      val testCode: String =
        s"""$ToolboxTestImports
           |
           |tmsFor[Option[Int => Int]]$options {
           |  (i: Int) => Some(1) + i
           |}
           |
           |""".stripMargin

      evaluateCode(testCode).asInstanceOf[Some[Int => Int]].get.apply(2) shouldBe 3
    }

    it should "process tts in function with placeholder body" in new ToolboxTest {
      val testCode: String =
        s"""$ToolboxTestImports
           |
           |tmsFor[Option[Int => Int]]$options {
           |  Some(1) + (_: Int)
           |}
           |
           |""".stripMargin

      evaluateCode(testCode).asInstanceOf[Some[Int => Int]].get.apply(2) shouldBe 3
    }

    it should "process partial function with case value access in the body & tts in the body" in new ToolboxTest {
      val testCode: String =
        s"""$ToolboxTestImports
           |
           |tmsFor[Option[PartialFunction[Int, Int]]]$options {
           |  {
           |    case i if false => i + 1
           |    case _: Int => Some(1) + 2
           |  }: PartialFunction[Int, Int]
           |}
           |
           |""".stripMargin

      val somePf = evaluateCode(testCode).asInstanceOf[Some[PartialFunction[Int, Unit]]]
      somePf.get.apply(2) shouldBe {}
    }

    it should "process tts in partial function case guard & body with case value access" in new ToolboxTest {
      val testCode: String =
        s"""$ToolboxTestImports
           |
           |tmsFor[Option[PartialFunction[Int, Int]]]$options {
           |  {
           |    case i: Int if i > Some(0) => 1
           |    case i: Int if i <= 0 => Some(0) - i
           |  }: PartialFunction[Int, Int]
           |}
           |
           |""".stripMargin

      val somePf = evaluateCode(testCode).asInstanceOf[Some[PartialFunction[Int, Int]]]
      somePf.get.apply(2) shouldBe 1
      somePf.get.apply(-2) shouldBe 2
    }

    it should "process tts in partial function case body that is multi-expression with & without parentheses" in new ToolboxTest {
      val testCode: String =
        s"""$ToolboxTestImports
           |
           |tmsFor[Option[PartialFunction[Int, Int]]]$options {
           |  {
           |    case i if i > 0 =>
           |      Some(1) - 0
           |      i
           |    case i => {
           |      Some(1) - 0
           |      i
           |    }
           |  }: PartialFunction[Int, Int]
           |}
           |
           |""".stripMargin

      val somePf = evaluateCode(testCode).asInstanceOf[Some[PartialFunction[Int, Int]]]
      somePf.get.apply(2) shouldBe 2
      somePf.get.apply(-2) shouldBe -2
    }

    it should "process tts in match expression" in new ToolboxTest {
      val testCode: String =
        s"""$ToolboxTestImports
           |
           |tmsFor[Option[Int]]$options {
           |  (Some(-5) + 2) match {
           |    case _ => -1
           |  }
           |}
           |
           |""".stripMargin

      evaluateCode(testCode) shouldBe Some(-1)
    }

    it should "process tts in match with bind val case guard & body" in new ToolboxTest {
      val testCode: String =
        s"""$ToolboxTestImports
           |
           |tmsFor[Option[Int]]$options {
           |  -5 match {
           |    case i if i > Some(0) => i
           |    case i@_ if i < 0 => Some(5) + i
           |    case i: Int => -i
           |  }
           |}
           |
           |""".stripMargin

      evaluateCode(testCode) shouldBe Some(0)
    }

    // ??? it could not refine the type of the input match ??? Its type stays Any and this match expression may not be flatMapped
    // in Scala 2.11 & 2.12.12-:
    // <toolbox>:10: warning: failed to refine type of the input code. Exception: _type application is not allowed in pattern_
    //  Some(-5) match {
    //           ^
    // in Scala 2.13 toolbox does not emit warnings, but non-toolbox test warns: failed to refine type of the input code. Exception: _'=>' expected but ':' found._
    // see non-toolbox test with solution: assign match to value and return it
    it should "process tts in match with unapply bind val case guard & body" in new ToolboxTest {
      val testCode: String =
        s"""$ToolboxTestImports
           |
           |tmsFor[Option[Int]]$options {
           |  Some(-5) match { // emits warning: failed to refine type of the input code. ...
           |    case Some(i) if i > Some(0) => i
           |    case Some(i@_) if i < 0 => Some(5) + i
           |    case Some(i: Int) => -i
           |    case _ => -1
           |  }
           |}
           |
           |""".stripMargin

      evaluateCode(testCode) shouldBe Some(0)
    }

    //see comments to the test above
    it should "process tts in match with unapply bind val case guard & body - no warning solution" in new ToolboxTest {
      val testCode: String =
        s"""$ToolboxTestImports
           |
           |tmsFor[Option[Int]]$options {
           |  val matchValue = Some(-5) match {
           |    case Some(i) if i > Some(0) => i
           |    case Some(i@_) if i < 0 => Some(5) + i
           |    case Some(i: Int) => -i
           |    case _ => -1
           |  }
           |  matchValue
           |}
           |
           |""".stripMargin

      evaluateCode(testCode) shouldBe Some(0)
    }

    //see comments to the test above
    it should "process tts in match with unapply bind val case guard & body: NON-TOOLBOX" in {
      {
        import sands.sugar.tms.TransparentMonads._

        tmsFor[Option[Int]]() {
          Some(-5) match { // warning: failed to refine type of the input code. ...
            case Some(i) if i > Some(0) => i
            case Some(i@_) if i < 0 => Some(5) + i
            case Some(i: Int) => -i
            case _ => -1
          }
        }
      } shouldBe Some(0)
    }

    //see comments to the test above
    it should "process tts in match with unapply bind val case guard & body - no warning solution: NON-TOOLBOX" in {
      {
        import sands.sugar.tms.TransparentMonads._

        tmsFor[Option[Int]]() {
          val matchValue = Some(-5) match {
            case Some(i) if i > Some(0) => i
            case Some(i@_) if i < 0 => Some(5) + i
            case Some(i: Int) => -i
            case _ => -1
          }
          matchValue
        }
      } shouldBe Some(0)
    }

    it should "process tts in try-catch expression" in new ToolboxTest {
      val testCode: String =
        s"""$ToolboxTestImports
           |
           |tmsFor[Option[Int]]$options {
           |  try Some(1)/0
           |  catch {
           |    case th : Throwable =>
           |      th.getMessage
           |      0
           |  }
           |}
           |
           |""".stripMargin

      evaluateCode(testCode) shouldBe Some(0)
    }

    it should "process tts in try-catch case guard" in new ToolboxTest {
      val testCode: String =
        s"""$ToolboxTestImports
           |
           |tmsFor[Option[Int]]$options {
           |  try 1/0
           |  catch {
           |    case _ : Throwable if Some(true) => 1
           |  }
           |}
           |
           |""".stripMargin

      evaluateCode(testCode) shouldBe Some(1)
    }

    it should "process tts in try-catch case body" in new ToolboxTest {
      val testCode: String =
        s"""$ToolboxTestImports
           |
           |tmsFor[Option[Int]]$options {
           |  try 1/0
           |  catch {
           |    case _ : Throwable => Some(1) + 9
           |  }
           |}
           |
           |""".stripMargin

      evaluateCode(testCode) shouldBe Some(10)
    }

    it should "process tts in try-catch finally" in new ToolboxTest {
      val testCode: String =
        s"""$ToolboxTestImports
           |
           |tmsFor[Option[Int]]$options {
           |  var a = 0
           |  try 1/0
           |  catch {
           |    case _ : Throwable => a = 1
           |  }
           |  finally {
           |    a = Some(1) + 1
           |  }
           |  a
           |}
           |
           |""".stripMargin

      evaluateCode(testCode) shouldBe Some(2)
    }

    it should "process tts in val initializer" in new ToolboxTest {
      val testCode: String =
        s"""$ToolboxTestImports
           |
           |tmsFor[Option[Int]]$options {
           |  val i: Int = Some(3) - 1
           |  i
           |}
           |
           |""".stripMargin

      evaluateCode(testCode) shouldBe Some(2)
    }

    it should "process val without tts in val initializer" in new ToolboxTest {
      val testCode: String =
        s"""$ToolboxTestImports
           |
           |tmsFor[Option[Int]]$options {
           |  val i = Some(3)
           |  Some(1) + 1
           |  i
           |}
           |""".stripMargin

      evaluateCode(testCode) shouldBe Some(3)
    }

    it should "process val with extractor pattern" in new ToolboxTest {
      val testCode: String =
        s"""$ToolboxTestImports
           |
           |tmsFor[Option[Int]]$options {
           |  val s@Some(v) = Some(1)
           |  Some(0) + 0
           |  s.get + v
           |}
           |""".stripMargin

      evaluateCode(testCode) shouldBe Some(2)
    }

    //2.11 toolbox: reflective compilation has failed: not found: value i$lzy
    it should "process tts in lazy val initializer: NON-TOOLBOX" in {
      {
        import sands.sugar.tms.TransparentMonads._

        tmsFor[Option[Int]]() {
          lazy val i: Int = Some(3) - 1
          i
        }
      } shouldBe Some(2)
    }

    it should "process tts in var initializer" in new ToolboxTest {
      val testCode: String =
        s"""$ToolboxTestImports
           |
           |tmsFor[Option[Int]]$options {
           |  var i: Int = Some(3) - 1
           |  i
           |}
           |
           |""".stripMargin

      evaluateCode(testCode) shouldBe Some(2)
    }

    it should "process tts in def accessor body" in new ToolboxTest {
      val testCode: String =
        s"""$ToolboxTestImports
           |
           |tmsFor[Option[Int]]$options {
           |  def i: Int = Some(3) - 1
           |  i
           |}
           |
           |""".stripMargin

      evaluateCode(testCode) shouldBe Some(2)
    }

    it should "process tts in def method body" in new ToolboxTest {
      val testCode: String =
        s"""$ToolboxTestImports
           |
           |tmsFor[Option[Int]]$options {
           |  def twiceSum(i: Int)(j: Int): Int = Some(2) * (i + j)
           |
           |  twiceSum(3)(4)
           |}
           |
           |""".stripMargin

      evaluateCode(testCode) shouldBe Some(14)
    }

    //<toolbox>:12: error: in object __wrapper$1$fc46f87bdf614962a334e64a1e06e3ac, multiple overloaded alternatives of sumInts define default arguments
    it should "process tts in def parameter default value & having variable length arg: NON-TOOLBOX" in {
      {
        import sands.sugar.tms.TransparentMonads._

        tmsFor[Option[Int]]() {
          def sumInts(arg: Int = Some(2) - 1)(args: Int*): Int = arg + args.sum

          sumInts()(2, 3)
        }
      } shouldBe Some(6)
    }

    // *** Local Trait definition & instantiation

    it should "process empty trait definition & instantiation without tts" in new ToolboxTest {
      val testCode: String =
        s"""$ToolboxTestImports
           |
           |tmsFor[Option[AnyRef]]$options {
           |  trait TestTrait
           |  Some(0) + 0 // stub to be tms
           |  new TestTrait {}
           |}
           |
           |""".stripMargin

      evaluateCode(testCode).asInstanceOf[Option[AnyRef]].get.getClass.getName should include ("$anon")
    }

    it should "process empty trait definition & instantiation with tts in refinement" in new ToolboxTest {
      val testCode: String =
        s"""$ToolboxTestImports
           |
           |tmsFor[Option[Int]]$options {
           |  trait TestTrait
           |
           |  new TestTrait {
           |    val i: Int = Some(5)
           |    def j = i + Some(7)
           |  }.j
           |}
           |
           |""".stripMargin

      evaluateCode(testCode) shouldBe Some(12)
    }

    it should "process trait with field definition & instantiation without tts" in new ToolboxTest {
      val testCode: String =
        s"""$ToolboxTestImports
           |
           |tmsFor[Option[Int]]$options {
           |  trait TestTrait {
           |    val i: Int = 3
           |  }
           |  Some(0) + 0 // stub to be tms
           |  new TestTrait {}.i
           |}
           |
           |""".stripMargin

      evaluateCode(testCode) shouldBe Some(3)
    }

    it should "process trait with field definition & instantiation with tts in field value" in new ToolboxTest {
      val testCode: String =
        s"""$ToolboxTestImports
           |
           |tmsFor[Option[Int]]$options {
           |  trait TestTrait {
           |    val i: Int = Some(3)
           |  }
           |  new TestTrait {}.i
           |}
           |
           |""".stripMargin

      evaluateCode(testCode) shouldBe Some(3)
    }

    it should "process trait extending trait definition & instantiation without tts" in new ToolboxTest {
      val testCode: String =
        s"""$ToolboxTestImports
           |
           |trait Trait {
           |  def i: Int = 3
           |}
           |
           |tmsFor[Option[Int]]$options {
           |  trait TestTrait extends Trait
           |  Some(0) + 0 // stub to be tms
           |  new TestTrait {}.i
           |}
           |
           |""".stripMargin

      evaluateCode(testCode) shouldBe Some(3)
    }

    //toolbox:reflective compilation has failed:
    //illegal inheritance; superclass Any
    // is not a subclass of the superclass Parent
    // of the mixin trait TestTrait
    //Any does not have a constructor
    it should "process trait extending class definition & instantiation without tts: NON-TOOLBOX" in {
      {
        import sands.sugar.tms.TransparentMonads._

        class Parent(val j: Int = 0)

        tmsFor[Option[Int]]() {
          trait TestTrait extends Parent
          Some(0) + 0 // stub to be tms
          new TestTrait {}.j
        }
      } shouldBe Some(0)
    }

    it should "process trait with refinement body definition & instantiation with tts in the refinement body" in new ToolboxTest {
      val testCode: String =
        s"""$ToolboxTestImports
           |
           |tmsFor[Option[Int]]$options {
           |  trait TestTrait {
           |    def j(add: Int): Int = Some(1) + add
           |  }
           |  new TestTrait {}.j(4)
           |}
           |
           |""".stripMargin

      evaluateCode(testCode) shouldBe Some(5)
    }

    it should "process trait with early definitions definition & instantiation with tts in the early definitions" in new ToolboxTest {
      val testCode: String =
        s"""$ToolboxTestImports
           |
           |tmsFor[Option[Int]]$options {
           |  trait TestTrait extends {
           |    val e: Int = Some(1)
           |  }
           |  new TestTrait {}.e
           |}
           |
           |""".stripMargin

      evaluateCode(testCode) shouldBe Some(1)
    }

    if (ScalaStringVersion >= "2.12") {
      // 2.11: reflective typecheck has failed: `lazy' definitions may not be initialized early
      it should "process trait with early definitions extending trait with refinement body definition with self & instantiation with tts in everything" in new ToolboxTest {
        val testCode: String =
          s"""$ToolboxTestImports
             |
             |trait ParentTrait { def k: Int }
             |
             |tmsFor[Option[Int]]$options {
             |  trait TestTrait extends {
             |    lazy val j: Int = Some(2)
             |  } with ParentTrait { self =>
             |    def k: Int = Some(10)
             |  }
             |  new TestTrait {}.k
             |}
             |
             |""".stripMargin

        evaluateCode(testCode) shouldBe Some(10)
      }
    }

    // *** Local Class definition & instantiation

    it should "process class without fields definition & instantiation without tts" in new ToolboxTest {
      val testCode: String =
        s"""$ToolboxTestImports
           |
           |tmsFor[Option[AnyRef]]$options {
           |  class TestClass()
           |  Some(0) + 0 // stub to be tms
           |  new TestClass()
           |}
           |
           |""".stripMargin

      evaluateCode(testCode).asInstanceOf[Option[AnyRef]].get.getClass.getName should include ("TestClass")
    }

    it should "process class definition after instantiation without tts" in new ToolboxTest {
      val testCode: String =
        s"""$ToolboxTestImports
           |
           |tmsFor[Option[Int]]$options {
           |  new TestClass()
           |  class TestClass()
           |  Some(1) + 2 // stub to be tms
           |}
           |
           |""".stripMargin

      evaluateCode(testCode) shouldBe Some(3)
    }

    it should "process class with field definition & instantiation without tts" in new ToolboxTest {
      val testCode: String =
        s"""$ToolboxTestImports
           |
           |tmsFor[Option[AnyRef]]$options {
           |  class TestClass(i: Int)
           |  Some(0) + 0 // stub to be tms
           |  new TestClass(3)
           |}
           |
           |""".stripMargin

      evaluateCode(testCode).asInstanceOf[Option[AnyRef]].get.getClass.getName should include ("TestClass")
    }

    it should "process class with val field definition & instantiation without tts" in new ToolboxTest {
      val testCode: String =
        s"""$ToolboxTestImports
           |
           |tmsFor[Option[Int]]$options {
           |  class TestClass(val i: Int)
           |  Some(0) + 0 // stub to be tms
           |  new TestClass(3).i
           |}
           |
           |""".stripMargin

      evaluateCode(testCode) shouldBe Some(3)
    }

    //both tests in toolbox & non-toolbox insuperable error: reflective typecheck has failed: called constructor's definition must precede calling constructor's definition
//    it should "process class with constructor definition & instantiation without tts" in new MacroTest {
//      val testCode: String =
//        s"""$importsAndVals
//           |
//           |tmsFor[Option[Int]]$options {
//           |  class TestClass(i: Int, j: Int) {
//           |    def this(m: Int) = this(m, 5)
//           |    def sum: Int = i + j
//           |  }
//           |  Some(0) + 0 // stub to be tms
//           |  new TestClass(3).sum
//           |}
//           |
//           |""".stripMargin
//
//      evaluateCode(testCode) shouldBe Some(8)
//    }
//
//    it should "process class with constructor definition & instantiation with tts in constructor call" in new MacroTest {
//      val testCode: String =
//        s"""$importsAndVals
//           |
//           |tmsFor[Option[Int]]$options {
//           |  class TestClass(i: Int, j: Int) {
//           |    def this(m: Int) = this(m, Some(5))
//           |    def sum: Int = i + j
//           |  }
//           |  new TestClass(2).sum
//           |}
//           |
//           |""".stripMargin
//
//      evaluateCode(testCode) shouldBe Some(7)
//    }

    it should "process class with variable fields number definition & instantiation with tts" in new ToolboxTest {
      val testCode: String =
        s"""$ToolboxTestImports
           |
           |tmsFor[Option[Int]]$options {
           |  class TestClass(i: Int *) {
           |    def sum: Int = i.sum
           |  }
           |  val tc2 = new TestClass(1, Some(2))
           |  new TestClass().sum + tc2.sum
           |}
           |
           |""".stripMargin

      evaluateCode(testCode) shouldBe Some(3)
    }

    it should "process class with several fields list definition & instantiation with tts" in new ToolboxTest {
      val testCode: String =
        s"""$ToolboxTestImports
           |
           |tmsFor[Option[Int]]$options {
           |  class TestClass(i: Int)(j: Int) {
           |    def sum: Int = i + j
           |  }
           |  new TestClass(Some(1))(Some(2)).sum
           |}
           |
           |""".stripMargin

      evaluateCode(testCode) shouldBe Some(3)
    }

    it should "process class with default value definition & instantiation without tts at all (with non default creation): NON-TOOLBOX" in {
      {
        import sands.sugar.tms.TransparentMonads._

        tmsFor[Option[AnyRef]]() {
          class TestClass(i: Int = 0)
          Some(0) + 0 // stub to be tms
          new TestClass(5)
        }
      }.get.getClass.getName should include ("TestClass")
    }

    it should "process class with default value definition & instantiation without tts at all (with default creation): NON-TOOLBOX" in {
      {
        import sands.sugar.tms.TransparentMonads._

        tmsFor[Option[AnyRef]]() {
          class TestClass(i: Int = 0)
          Some(0) + 0 // stub to be tms
          new TestClass()
        }
      }.get.getClass.getName should include ("TestClass")
    }

    //<toolbox>:12: error: double definition:
    //def <init>$default$1 : Int at line 12 and
    //def <init>$default$1 : Int at line 12
    //have same type
    it should "process class with default value definition & instantiation without tts (with non default creation): NON-TOOLBOX" in {
      {
        import sands.sugar.tms.TransparentMonads._

        tmsFor[Option[AnyRef]]() {
          class TestClass(i: Int = 0)
          Some(0) + 0 // stub to be tms
          new TestClass(5)
        }
      }.get.getClass.getName should include ("TestClass")
    }

    //<toolbox>:12: error: in object TestClass, multiple overloaded alternatives of <init> define default arguments
    it should "process class with default value definition & instantiation without tts (with default creation): NON-TOOLBOX" in {
      {
        import sands.sugar.tms.TransparentMonads._

        tmsFor[Option[AnyRef]]() {
          class TestClass(i: Int = 0)
          Some(0) + 0 // stub to be tms
          new TestClass()
        }
      }.get.getClass.getName should include ("TestClass")
    }

    //<toolbox>:12: error: double definition:
    //def <init>$default$1 : Int at line 12 and
    //def <init>$default$1 : Int at line 12
    //have same type
    it should "process class with default value definition & instantiation with tts in default value (with non default creation): NON-TOOLBOX" in {
      {
        import sands.sugar.tms.TransparentMonads._

        tmsFor[Option[AnyRef]]() {
          class TestClass(i: Int = Some(1))
          new TestClass(0)
        }
      }.get.getClass.getName should include ("TestClass")
    }

    it should "process class with default value definition & instantiation with tts in default value (with default creation): NON-TOOLBOX" in {
      {
        import sands.sugar.tms.TransparentMonads._

        tmsFor[Option[AnyRef]]() {
          class TestClass(i: Int = Some(1))
          new TestClass()
        }
      }.get.getClass.getName should include ("TestClass")
    }

    it should "process class with refinement body definition & instantiation with tts in default value (with default creation): NON-TOOLBOX" in {
      {
        import sands.sugar.tms.TransparentMonads._

        tmsFor[Option[Int]]() {
          class TestClass(i: Int = Some(1)) {
            val j: Int = i + 1
          }
          new TestClass().j
        }
      } shouldBe Some(2)
    }

    it should "process class extending trait definition & instantiation with tts in default value (with non default creation): NON-TOOLBOX" in {
      {
        import sands.sugar.tms.TransparentMonads._

        trait TestTrait {
          def j: Int = 3
        }

        tmsFor[Option[Int]]() {
          class TestClass(i: Int = Some(1)) extends TestTrait
          new TestClass(0).j
        }
      } shouldBe Some(3)
    }

    it should "process class extending class definition & instantiation without tts" in new ToolboxTest {
      val testCode: String =
        s"""$ToolboxTestImports
           |
           |class Parent(val j: Int)
           |
           |tmsFor[Option[Int]]$options {
           |  class TestClass(i: Int) extends Parent(1)
           |  Some(1) + 0 // to be tms
           |  new TestClass(3).j
           |}
           |
           |""".stripMargin

      evaluateCode(testCode) shouldBe Some(1)
    }

    it should "process class extending class definition & instantiation with tts in extending class init" in new ToolboxTest {
      val testCode: String =
        s"""$ToolboxTestImports
           |
           |class Parent(val j: Int)
           |
           |tmsFor[Option[Int]]$options {
           |  class TestClass(i: Int) extends Parent(Some(1))
           |  new TestClass(3).j
           |}
           |
           |""".stripMargin

      evaluateCode(testCode) shouldBe Some(1)
    }

    it should "process class extending class definition & instantiation with tts in default value (with non default creation): NON-TOOLBOX" in {
      {
        import sands.sugar.tms.TransparentMonads._

        class Parent(val j: Int = 0)

        tmsFor[Option[Int]]() {
          class TestClass(i: Int = Some(1)) extends Parent(2)
          new TestClass(0).j
        }
      } shouldBe Some(2)
    }

    it should "process class extending class definition & instantiation with tts in default value (both with default creation): NON-TOOLBOX" in {
      {
        import sands.sugar.tms.TransparentMonads._

        class Parent(val j: Int = 0)

        tmsFor[Option[Int]]() {
          class TestClass(i: Int = Some(1)) extends Parent()
          new TestClass().j
        }
      } shouldBe Some(0)
    }

    it should "process class extending class with refinement body definition & instantiation with tts in default value (both with default creation): NON-TOOLBOX" in {
      {
        import sands.sugar.tms.TransparentMonads._

        class Parent(j: Int = 0)

        tmsFor[Option[Int]]() {
          class TestClass(i: Int = Some(1)) extends Parent() {
            def k: Int = i + 2
          }
          new TestClass().k
        }
      } shouldBe Some(3)
    }

    it should "process class with refinement body definition & instantiation with tts in the refinement body" in new ToolboxTest {
      val testCode: String =
        s"""$ToolboxTestImports
           |
           |tmsFor[Option[Int]]$options {
           |  class TestClass(i: Int) {
           |    def j(add: Int): Int = Some(1) + add
           |  }
           |  new TestClass(3).j(4)
           |}
           |
           |""".stripMargin

      evaluateCode(testCode) shouldBe Some(5)
    }

    it should "process class with early definitions definition & instantiation with tts in the early definitions" in new ToolboxTest {
      val testCode: String =
        s"""$ToolboxTestImports
           |
           |tmsFor[Option[Int]]$options {
           |  class TestClass(i: Int) extends {
           |    val e: Int = Some(1)
           |  }
           |  new TestClass(3).e
           |}
           |
           |""".stripMargin

      evaluateCode(testCode) shouldBe Some(1)
    }

    it should "process class with early definitions extending class definition & instantiation with tts in the early definitions" in new ToolboxTest {
      val testCode: String =
        s"""$ToolboxTestImports
           |
           |class Parent(val j: Int)
           |
           |tmsFor[Option[Int]]$options {
           |  class TestClass(i: Int) extends {
           |    val e: Int = Some(1)
           |  } with Parent(e)
           |  new TestClass(3).j
           |}
           |
           |""".stripMargin

      evaluateCode(testCode) shouldBe Some(1)
    }

    it should "process class with early definitions extending trait with refinement body definition with self & instantiation with tts in everything" in new ToolboxTest {
      val testCode: String =
        s"""$ToolboxTestImports
           |
           |trait ParentTrait { def k: Int }
           |
           |tmsFor[Option[Int]]$options {
           |  class TestClass(i: Int) extends {
           |    val j: Int = Some(2)
           |  } with ParentTrait { self =>
           |    def k: Int = self.j + self.i + Some(10)
           |  }
           |  new TestClass(3).k
           |}
           |
           |""".stripMargin

      evaluateCode(testCode) shouldBe Some(15)
    }

    it should "process class with early definitions extending trait with refinement body definition with self & instantiation with tts in everything: NON-TOOLBOX" in {
      {
        import sands.sugar.tms.TransparentMonads._

        trait ParentTrait { def k: Int }

        tmsFor[Option[Int]]() {
          class TestClass(i: Int) extends {
            val j: Int = Some(2)
          } with ParentTrait { self =>
            def k: Int = Some(10) + self.j + self.i
          }
          new TestClass(3).k
        }
      } shouldBe Some(15)
    }

    //<toolbox>:15: error: in object TestClass, multiple overloaded alternatives of <init> define default arguments
    it should "process class with early definitions extending class with trait with refinement body definition with self & instantiation with tts in everything & default value: NON-TOOLBOX" in {
      {
        import sands.sugar.tms.TransparentMonads._

        class Parent(val j: Int)
        trait ParentTrait { def k: Int }

        tmsFor[Option[Int]]() {
          class TestClass(i: Int = Some(1)) extends {
            val e: Int = Some(2)
          } with Parent(Some(3)) with ParentTrait { self =>
            def k: Int = Some(4) + self.j + self.i + self.e
          }
          new TestClass().k
        }
      } shouldBe Some(10)
    }

    it should "process class with early definitions extending trait with refinement body definition with self & instantiation with tts in everything & default value: NON-TOOLBOX" in {
      {
        import sands.sugar.tms.TransparentMonads._

        trait ParentTrait { def k: Int }

        tmsFor[Option[Int]]() {
          class TestClass(i: Int = Some(3)) extends {
            val j: Int = Some(2)
          } with ParentTrait { self =>
            def k: Int = Some(10) + self.j + self.i
          }
          new TestClass().k
        }
      } shouldBe Some(15)
    }

    // *** Local Case Class definition is not possible in general: private[this] not allowed for case class parameters

    // *** new Trait

    it should "process new empty trait without tts" in new ToolboxTest {
      val testCode: String =
        s"""$ToolboxTestImports
           |
           |trait TestTrait
           |
           |tmsFor[Option[AnyRef]]$options {
           |  Some(0) + 0 // stub to be tms
           |  new TestTrait {}
           |}
           |
           |""".stripMargin

      evaluateCode(testCode).asInstanceOf[Option[AnyRef]].get.getClass.getName should include ("anon")
    }

    it should "process new trait with field definition without tts" in new ToolboxTest {
      val testCode: String =
        s"""$ToolboxTestImports
           |
           |trait TestTrait {
           |  val i: Int = 3
           |}
           |
           |tmsFor[Option[Int]]$options {
           |  Some(0) + 0 // stub to be tms
           |  new TestTrait {}.i
           |}
           |
           |""".stripMargin

      evaluateCode(testCode) shouldBe Some(3)
    }

    it should "process new trait with tts in refinement body" in new ToolboxTest {
      val testCode: String =
        s"""$ToolboxTestImports
           |
           |trait TestTrait { def i: Int }
           |
           |tmsFor[Option[Int]]$options {
           |  new TestTrait {
           |    def i: Int = Some(0)
           |  }.i
           |}
           |
           |""".stripMargin

      evaluateCode(testCode) shouldBe Some(0)
    }

    it should "process new trait with other trait with tts in refinement body" in new ToolboxTest {
      val testCode: String =
        s"""$ToolboxTestImports
           |
           |trait TestTrait { def i: Int }
           |trait Trait
           |
           |tmsFor[Option[Int]]$options {
           |  new TestTrait with Trait {
           |    def i: Int = 3
           |    Some(0) + 1
           |  }.i
           |}
           |
           |""".stripMargin

      evaluateCode(testCode) shouldBe Some(3)
    }

    it should "process new trait with other trait with tts in refinement body with self" in new ToolboxTest {
      val testCode: String =
        s"""$ToolboxTestImports
           |
           |trait TestTrait { def i: Int }
           |trait Trait { val j: Int = 3 }
           |
           |tmsFor[Option[Int]]$options {
           |  new TestTrait with Trait { self =>
           |    def i: Int = self.j + Some(1)
           |  }.i
           |}
           |
           |""".stripMargin

      evaluateCode(testCode) shouldBe Some(4)
    }

    it should "process new generic trait with other trait with tts in refinement body" in new ToolboxTest {
      val testCode: String =
        s"""$ToolboxTestImports
           |
           |trait TestTrait[T] { def i: T }
           |trait Trait[T]
           |
           |tmsFor[Option[Int]]$options {
           |  new TestTrait[Int] with Trait[String] {
           |    def i: Int = 3
           |    Some(0) + 1
           |  }.i
           |}
           |
           |""".stripMargin

      evaluateCode(testCode) shouldBe Some(3)
    }

    it should "process new trait with tts in early definitions" in new ToolboxTest {
      val testCode: String =
        s"""$ToolboxTestImports
           |
           |trait TestTrait {
           |  val i: Int = 3
           |}
           |
           |tmsFor[Option[Int]]$options {
           |  (new {
           |    val e: Int = Some(5)
           |  } with TestTrait).i
           |}
           |
           |""".stripMargin

      evaluateCode(testCode) shouldBe Some(3)
    }

    it should "process new abstract trait with tts in early definitions" in new ToolboxTest {
      val testCode: String =
        s"""$ToolboxTestImports
           |
           |trait TestTrait {
           |  val e: Int
           |}
           |
           |tmsFor[Option[Int]]$options {
           |  (new {
           |    val e: Int = Some(5)
           |  } with TestTrait).e
           |}
           |
           |""".stripMargin

      evaluateCode(testCode) shouldBe Some(5)
    }

    it should "process new generic abstract trait with tts in early definitions" in new ToolboxTest {
      val testCode: String =
        s"""$ToolboxTestImports
           |
           |trait TestTrait[T] {
           |  val e: T
           |}
           |
           |tmsFor[Option[Int]]$options {
           |  (new {
           |    val e: Int = Some(5)
           |  } with TestTrait[Int]).e
           |}
           |
           |""".stripMargin

      evaluateCode(testCode) shouldBe Some(5)
    }

    it should "process new generic trait with early definitions with other trait with tts in everything" in new ToolboxTest {
      val testCode: String =
        s"""$ToolboxTestImports
           |
           |trait TestTrait[T] { def i: T }
           |trait Trait[T]
           |
           |tmsFor[Option[Int]]$options {
           |  new {
           |    val e: Int = Some(5)
           |  } with TestTrait[Int] with Trait[String] {
           |    def i: Int = e
           |    Some(0) + 1
           |  }.i
           |}
           |
           |""".stripMargin

      evaluateCode(testCode) shouldBe Some(5)
    }

    // *** new Anonymous Class

    it should "process new empty anonymous class" in new ToolboxTest {
      val testCode: String =
        s"""$ToolboxTestImports
           |
           |tmsFor[Option[AnyRef]]$options {
           |  Some(0) + 0 // stub to be tms
           |  new {}
           |}
           |
           |""".stripMargin

      evaluateCode(testCode).asInstanceOf[Option[AnyRef]].get.getClass.getName should include ("anon")
    }

    it should "process new anonymous class with tts in body" in new ToolboxTest {
      val testCode: String =
        s"""$ToolboxTestImports
           |
           |tmsFor[Option[Int]]$options {
           |  new {
           |    def i: Int = Some(0)
           |  }.i
           |}
           |
           |""".stripMargin

      evaluateCode(testCode) shouldBe Some(0)
    }

    it should "process new anonymous class with tts in body with self" in new ToolboxTest {
      val testCode: String =
        s"""$ToolboxTestImports
           |
           |tmsFor[Option[Int]]$options {
           |  new { self =>
           |    def i: Int = Some(0)
           |    self.i
           |  }.i
           |}
           |
           |""".stripMargin

      evaluateCode(testCode) shouldBe Some(0)
    }

    // *** new Class

    it should "process new class without fields without tts" in new ToolboxTest {
      val testCode: String =
        s"""$ToolboxTestImports
           |
           |class TestClass()
           |
           |tmsFor[Option[AnyRef]]$options {
           |  Some(0) + 0 // stub to be tms
           |  new TestClass()
           |}
           |
           |""".stripMargin

      evaluateCode(testCode).asInstanceOf[Option[AnyRef]].get.getClass.getName should include ("TestClass")
    }

    it should "process new class with tts in parameter" in new ToolboxTest {
      val testCode: String =
        s"""$ToolboxTestImports
           |
           |class TestClass(val i: Int)
           |
           |tmsFor[Option[Int]]$options {
           |  (new TestClass(Some(5))).i
           |}
           |
           |""".stripMargin

      evaluateCode(testCode) shouldBe Some(5)
    }

    it should "process new class with other trait with tts in parameter" in new ToolboxTest {
      val testCode: String =
        s"""$ToolboxTestImports
           |
           |class TestClass(val i: Int)
           |trait TestTrait[T]
           |
           |tmsFor[Option[Int]]$options {
           |  (new TestClass(Some(5)) with TestTrait[String]).i
           |}
           |
           |""".stripMargin

      evaluateCode(testCode) shouldBe Some(5)
    }

    it should "process new class with refinement body with tts in parameter" in new ToolboxTest {
      val testCode: String =
        s"""$ToolboxTestImports
           |
           |class TestClass(val i: Int)
           |
           |tmsFor[Option[Int]]$options {
           |  (new TestClass(Some(5)) {
           |    val v = 3
           |  }).i
           |}
           |
           |""".stripMargin

      evaluateCode(testCode) shouldBe Some(5)
    }

    it should "process new class with refinement body with tts in parameter & refinement body " in new ToolboxTest {
      val testCode: String =
        s"""$ToolboxTestImports
           |
           |class TestClass(val i: Int)
           |
           |tmsFor[Option[Int]]$options {
           |  (new TestClass(Some(5)) {
           |    Some(3) + 1
           |  }).i
           |}
           |
           |""".stripMargin

      evaluateCode(testCode) shouldBe Some(5)
    }

    it should "process new class with 2 tts in parameters" in new ToolboxTest {
      val testCode: String =
        s"""$ToolboxTestImports
           |
           |class TestClass(val i: Int)(j: Int)
           |
           |tmsFor[Option[Int]]$options {
           |  new TestClass(Some(5))(Some(6)).i
           |}
           |
           |""".stripMargin

      evaluateCode(testCode) shouldBe Some(5)
    }

    if (ScalaStringVersion >= "2.12") {
      // 2.11: reflective compilation has failed: value i overrides nothing
      it should "process new class with early definitions with tts in early definitions" in new ToolboxTest {
        val testCode: String =
          s"""$ToolboxTestImports
             |
             |class TestClass(val i: Int) {
             |  def twice: Int = 2 * i
             |}
             |
             |tmsFor[Option[Int]]$options {
             |  (new {
             |    override val i: Int = Some(1)
             |  } with TestClass(3)).twice
             |}
             |
             |""".stripMargin

        evaluateCode(testCode) shouldBe Some(2)
      }

      // in 2.11
      // [info]   scala.tools.reflect.ToolBoxError: reflective compilation has failed:
      // [info] TestClass is not an enclosing class
      it should "process new generic class with 2 tts in parameters" in new ToolboxTest {
        val testCode: String =
          s"""$ToolboxTestImports
             |
             |class TestClass[T](val i: T)(j: T)
             |
             |tmsFor[Option[Int]]$options {
             |  new TestClass[Int](Some(5))(Some(6)).i
             |}
             |
             |""".stripMargin

        evaluateCode(testCode) shouldBe Some(5)
      }

      // in 2.11
      // <toolbox>:14: error: value i overrides nothing
      it should "process new class with tts in refinement body with override" in new ToolboxTest {
        val testCode: String =
          s"""$ToolboxTestImports
             |
             |class TestClass(val i: Int)
             |
             |tmsFor[Option[Int]]$options {
             |  (new TestClass(5) {
             |    override val i: Int = Some(1) + 2
             |  }).i
             |}
             |
             |""".stripMargin

        evaluateCode(testCode) shouldBe Some(3)
      }

      // 2.11: <toolbox>:12: error: TestClass is not an enclosing class
      //class TestClass[T](val i: T)(j: T)
      it should "process new generic class with tts in parameters and in refinement body with override" in new ToolboxTest {
        val testCode: String =
          s"""$ToolboxTestImports
             |
             |class TestClass[T](val i: T)(j: T)
             |
             |tmsFor[Option[Int]]$options {
             |  new TestClass[Int](Some(5))(Some(6)) {
             |    override val i: Int = Some(3)
             |  }.i
             |}
             |
             |""".stripMargin

        evaluateCode(testCode) shouldBe Some(3)
      }

      // 2.11: <toolbox>:12: error: TestClass is not an enclosing class
      //class TestClass[T](val i: T)(j: T)
      it should "process new generic class with tts in parameters and in refinement body without override" in new ToolboxTest {
        val testCode: String =
          s"""$ToolboxTestImports
             |
             |class TestClass[T](val i: T)(j: T)
             |
             |tmsFor[Option[Int]]$options {
             |  new TestClass[Int](Some(5))(Some(6)) {
             |    def ij: Int = Some(3)
             |  }.ij
             |}
             |
             |""".stripMargin

        evaluateCode(testCode) shouldBe Some(3)
      }
    }

    it should "process new class with refinement body with implicit conversion in parameter (Int => Long)" in new ToolboxTest {
      val testCode: String =
        s"""$ToolboxTestImports
           |
           |class TestClass(val i: Long)
           |
           |tmsFor[Option[Long]]$options {
           |  Some(new TestClass(5) {
           |    val v = 2
           |  }).i
           |}
           |
           |""".stripMargin

      evaluateCode(testCode) shouldBe Some(5)
    }

    it should "process new class with refinement body with implicit conversion in parameter (Int => Long) & tts in body" in new ToolboxTest {
      val testCode: String =
        s"""$ToolboxTestImports
           |
           |class TestClass(val i: Long)
           |
           |tmsFor[Option[Long]]$options {
           |  val tc = new TestClass(5) {
           |    Some(1) + 2
           |  }
           |  tc.i
           |}
           |
           |""".stripMargin

      evaluateCode(testCode) shouldBe Some(5)
    }

    it should "process new class with val with refinement body with implicit conversion in parameter (Int => Long) & tts in body" in new ToolboxTest {
      val testCode: String =
        s"""$ToolboxTestImports
           |
           |class TestClass(val i: Long)
           |
           |tmsFor[Option[Long]]$options {
           |  (new TestClass(5) {
           |    val j: Long = Some(1) + 2L
           |  }).i
           |}
           |
           |""".stripMargin

      evaluateCode(testCode) shouldBe Some(5)
    }

    it should "process new class with other trait with tts in refinement body" in new ToolboxTest {
      val testCode: String =
        s"""$ToolboxTestImports
           |
           |class TestClass(val i: Int)
           |trait TestTrait[T]
           |
           |tmsFor[Option[Int]]$options {
           |  (new TestClass(5) with TestTrait[String] {
           |    val s: Int = Some(1) + i
           |  }).s
           |}
           |
           |""".stripMargin

      evaluateCode(testCode) shouldBe Some(6)
    }

    it should "process new class with val parameter with refinement body with tts in parameter without accessing class value" in new ToolboxTest {
      val testCode: String =
        s"""$ToolboxTestImports
           |
           |class TestClass(val i: Int)
           |
           |tmsFor[Option[Int]]$options {
           |  new TestClass(Some(5)) {
           |    def j = 1
           |  }.i
           |}
           |""".stripMargin

      evaluateCode(testCode) shouldBe Some(5)
    }

    it should "process new class with refinement body with tts in parameter without tts accessing refinement" in new ToolboxTest {
      val testCode: String =
        s"""$ToolboxTestImports
           |
           |class TestClass(i: Int)
           |
           |tmsFor[Option[Int]]$options {
           |  new TestClass(Some(5)) {
           |    def j = 1
           |  }.j
           |}
           |""".stripMargin

      evaluateCode(testCode) shouldBe Some(1)
    }

    it should "process new class with variable fields number with tts" in new ToolboxTest {
      val testCode: String =
        s"""$ToolboxTestImports
           |
           |class TestClass(i: Int *) {
           |  def sum: Int = i.sum
           |}
           |
           |tmsFor[Option[Int]]$options {
           |  new TestClass().sum + new TestClass(1, Some(2)).sum
           |}
           |
           |""".stripMargin

      evaluateCode(testCode) shouldBe Some(3)
    }

    //<toolbox>:12: error: in object TestClass, multiple overloaded alternatives of <init> define default arguments
    it should "process new class with default field value without tts with non default creation: NON-TOOLBOX" in {
      {
        import sands.sugar.tms.TransparentMonads._

        class TestClass(i: Int = 0)

        tmsFor[Option[AnyRef]]() {
          Some(0) + 0 // stub to be tms
          new TestClass(5)
        }
      }.get.getClass.getName should include ("TestClass")
    }

    //<toolbox>:12: error: in object TestClass, multiple overloaded alternatives of <init> define default arguments
    it should "process new class with default field value without tts with default creation: NON-TOOLBOX" in {
      {
        import sands.sugar.tms.TransparentMonads._

        class TestClass(i: Int = 0)

        tmsFor[Option[AnyRef]]() {
          Some(0) + 0 // stub to be tms
          new TestClass()
        }
      }.get.getClass.getName should include ("TestClass")
    }

    // *** new Case Class
    // mostly NON-TOOLBOX since: <toolbox>: error: private[this] not allowed for case class parameters

    it should "process new case class Some without tts" in new ToolboxTest {
      val testCode: String =
        s"""$ToolboxTestImports
           |
           |tmsFor[Option[Int]]$options {
           |  new Some[Int](1) + 2
           |}
           |
           |""".stripMargin

      evaluateCode(testCode) shouldBe Some(3)
    }

    it should "process new case class Some with FQN without tts" in new ToolboxTest {
      val testCode: String =
        s"""$ToolboxTestImports
           |
           |tmsFor[Option[Int]]$options {
           |  new scala.Some[Int](1) + 2
           |}
           |
           |""".stripMargin

      evaluateCode(testCode) shouldBe Some(3)
    }

    it should "process new case class Some with tts in parameter" in new ToolboxTest {
      val testCode: String =
        s"""$ToolboxTestImports
           |
           |tmsFor[Option[Int]]$options {
           |  new Some[Int](Some(5))
           |}
           |
           |""".stripMargin

      evaluateCode(testCode) shouldBe Some(5)
    }

    it should "process new case class with 2 tts in parameters: NON-TOOLBOX" in {
      {
        import sands.sugar.tms.TransparentMonads._

        case class TestClass(i: Int)(j: Int)

        tmsFor[Option[Int]]() {
          new TestClass(Some(5))(Some(6)).i
        }
      } shouldBe Some(5)
    }

    it should "process new case class with trait with 2 tts in parameters: NON-TOOLBOX" in {
      {
        import sands.sugar.tms.TransparentMonads._

        case class TestClass[T](i: T)(j: T)
        trait Parent {
          val k: Int = 12
        }

        tmsFor[Option[Int]]() {
          (new TestClass[Int](Some(5))(Some(6)) with Parent).k
        }
      } shouldBe Some(12)
    }

    it should "process new case class without fields without tts: NON-TOOLBOX" in {
      {
        import sands.sugar.tms.TransparentMonads._

        case class TestClass()

        tmsFor[Option[AnyRef]]() {
          Some(0) + 0 // stub to be tms
          new TestClass()
        }
      }.get.getClass.getName should include ("TestClass")
    }

    it should "process new case class with refinement body with tts in parameter: NON-TOOLBOX" in {
      {
        import sands.sugar.tms.TransparentMonads._

        case class TestClass(i: Int)

        tmsFor[Option[Int]]() {
          (new TestClass(Some(5)) {
            def rb = 3
          }).i
        }
      } shouldBe Some(5)
    }

    it should "process new case class with other trait with tts in refinement body: NON-TOOLBOX" in {
      {
        import sands.sugar.tms.TransparentMonads._

        case class TestClass(i: Int)
        trait TestTrait[T]

        tmsFor[Option[Int]]() {
          (new TestClass(5) with TestTrait[String] {
            val s: Int = Some(1) + i
          }).s
        }
      } shouldBe Some(6)
    }

    it should "process new case class with variable fields number with tts: NON-TOOLBOX" in {
      {
        import sands.sugar.tms.TransparentMonads._

        case class TestClass(i: Int *) {
          def sum: Int = i.sum
        }

        tmsFor[Option[Int]]() {
          new TestClass().sum + new TestClass(1, Some(2)).sum
        }
      } shouldBe Some(3)
    }

    it should "process new case class with default field value without tts with non default creation: NON-TOOLBOX" in {
      {
        import sands.sugar.tms.TransparentMonads._

        case class TestClass(i: Int = 0)

        tmsFor[Option[AnyRef]]() {
          Some(0) + 0 // stub to be tms
          new TestClass(5)
        }
      }.get.getClass.getName should include ("TestClass")
    }

    it should "process new case class with default field value without tts with default creation: NON-TOOLBOX" in {
      {
        import sands.sugar.tms.TransparentMonads._

        case class TestClass(i: Int = 0)

        tmsFor[Option[AnyRef]]() {
          Some(0) + 0 // stub to be tms
          new TestClass()
        }
      }.get.getClass.getName should include ("TestClass")
    }

    it should "process case class default instantiation with 2 tts in parameters: NON-TOOLBOX" in {
      {
        import sands.sugar.tms.TransparentMonads._

        case class TestClass(i: Int)(j: Int)

        tmsFor[Option[Int]]() {
          TestClass(Some(5))(Some(6)).i
        }
      } shouldBe Some(5)
    }

    // *** Local Object creation

    it should "process object without fields creation without tts" in new ToolboxTest {
      val testCode: String =
        s"""$ToolboxTestImports
           |
           |tmsFor[Option[AnyRef]]$options {
           |  Some(0) + 0 // stub to be tms
           |  object TestObject
           |  TestObject
           |}
           |
           |""".stripMargin

      evaluateCode(testCode).asInstanceOf[Option[AnyRef]].get.getClass.getName should include ("TestObject$")
    }

    it should "process object with refinement body creation with tts in refinement body" in new ToolboxTest {
      val testCode: String =
        s"""$ToolboxTestImports
           |
           |tmsFor[Option[Int]]$options {
           |  object TestObject {
           |    val i: Int = Some(1)
           |  }
           |  TestObject.i
           |}
           |
           |""".stripMargin

      evaluateCode(testCode) shouldBe Some(1)
    }

    it should "process object with refinement body creation with tts in refinement body accessing object val" in new ToolboxTest {
      val testCode: String =
        s"""$ToolboxTestImports
           |
           |tmsFor[Option[Int]]$options {
           |  object TestObject {
           |    val i: Int = Some(1)
           |    def j(add: Int): Int = i + add
           |  }
           |  TestObject.j(2)
           |}
           |
           |""".stripMargin

      evaluateCode(testCode) shouldBe Some(3)
    }

    it should "process object extending empty trait without tts" in new ToolboxTest {
      val testCode: String =
        s"""$ToolboxTestImports
           |
           |trait ParentTrait
           |
           |tmsFor[Option[ParentTrait]]$options {
           |  Some(0) + 0 // stub to be tms
           |  object TestObject extends ParentTrait
           |  TestObject
           |}
           |
           |""".stripMargin

      evaluateCode(testCode).asInstanceOf[Option[AnyRef]].get.getClass.getName should include ("TestObject$")
    }

    it should "process object extending non-abstract) trait with full definition creation without tts" in new ToolboxTest {
      val testCode: String =
        s"""$ToolboxTestImports
           |
           |trait ParentTrait {
           |  def t: Int = 1
           |}
           |
           |tmsFor[Option[Int]]$options {
           |  Some(0) + 0 // stub to be tms
           |  object TestObject extends ParentTrait
           |  Some(TestObject.t)
           |}
           |
           |""".stripMargin

      evaluateCode(testCode) shouldBe Some(1)
    }

    it should "process object extending abstract trait with refinement body with self creation with tts in refinement body" in new ToolboxTest {
      val testCode: String =
        s"""$ToolboxTestImports
           |
           |trait ParentTrait { def t: Int }
           |
           |tmsFor[Option[Int]]$options {
           |  object TestObject extends ParentTrait { self =>
           |    def t: Int = Some(10)
           |  }
           |  TestObject.t
           |}
           |
           |""".stripMargin

      evaluateCode(testCode) shouldBe Some(10)
    }

    it should "process object with early definitions extending trait with refinement body with self creation with tts in everything" in new ToolboxTest {
      val testCode: String =
        s"""$ToolboxTestImports
           |
           |trait ParentTrait { def k: Int }
           |
           |tmsFor[Option[Int]]$options {
           |  object TestObject extends {
           |    val j: Int = Some(2)
           |  } with ParentTrait { self =>
           |    def k: Int = Some(10) + self.j
           |  }
           |  TestObject.k
           |}
           |
           |""".stripMargin

      evaluateCode(testCode) shouldBe Some(12)
    }

    it should "process object extending anonymous class creation with tts in anonymous class" in new ToolboxTest {
      val testCode: String =
        s"""$ToolboxTestImports
           |
           |tmsFor[Option[Int]]$options {
           |  object TestObject extends {
           |    val acv: Int = Some(1)
           |    def acd: Int = Some(1)
           |  }
           |  TestObject.acv + TestObject.acd
           |}
           |
           |""".stripMargin

      evaluateCode(testCode) shouldBe Some(2)
    }

    it should "process object extending anonymous class creation with tts in anonymous class accessing class val" in new ToolboxTest {
      val testCode: String =
        s"""$ToolboxTestImports
           |
           |tmsFor[Option[Int]]$options {
           |  object TestObject extends {
           |    val acv: Int = Some(1)
           |    def acd: Int = acv + Some(1)
           |  }
           |  TestObject.acd
           |}
           |
           |""".stripMargin

      evaluateCode(testCode) shouldBe Some(2)
    }

    it should "process object extending class creation without tts" in new ToolboxTest {
      val testCode: String =
        s"""$ToolboxTestImports
           |
           |class Parent(val j: Int)
           |
           |tmsFor[Option[Int]]$options {
           |  Some(0) + 0 // stub to be tms
           |  object TestObject extends Parent(1)
           |  Some(TestObject.j)
           |}
           |
           |""".stripMargin

      evaluateCode(testCode) shouldBe Some(1)
    }

    it should "process object with early definitions extending class creation with tts in the early definitions" in new ToolboxTest {
      val testCode: String =
        s"""$ToolboxTestImports
           |
           |class Parent(val j: Int)
           |
           |tmsFor[Option[Int]]$options {
           |  object TestObject extends {
           |    val e: Int = Some(1)
           |  } with Parent(0)
           |  TestObject.e
           |}
           |
           |""".stripMargin

      evaluateCode(testCode) shouldBe Some(1)
    }

    if (ScalaStringVersion >= "2.12") {
      //2.11 reflective compilation has failed: value j overrides nothing
      it should "process object with early definitions override extending class creation with tts in the early definitions" in new ToolboxTest {
        val testCode: String =
          s"""$ToolboxTestImports
             |
             |class Parent(val j: Int)
             |
             |tmsFor[Option[Int]]$options {
             |  object TestObject extends {
             |    override val j: Int = Some(1)
             |  } with Parent(0)
             |  TestObject.j
             |}
             |
             |""".stripMargin

        evaluateCode(testCode) shouldBe Some(1)
      }
    }

    it should "process object extending class creation with tts in extended class init" in new ToolboxTest {
      val testCode: String =
        s"""$ToolboxTestImports
           |
           |class Parent(val j: Int)
           |
           |tmsFor[Option[Int]]$options {
           |  object TestObject extends Parent(Some(2))
           |  TestObject.j
           |}
           |
           |""".stripMargin

      evaluateCode(testCode) shouldBe Some(2)
    }

    it should "process object with early definitions extending class creation with tts in class init" in new ToolboxTest {
      val testCode: String =
        s"""$ToolboxTestImports
           |
           |class Parent(val j: Int)
           |
           |tmsFor[Option[Int]]$options {
           |  object TestObject extends {
           |    val e: Int = 1
           |  } with Parent(Some(1))
           |  TestObject.j
           |}
           |
           |""".stripMargin

      evaluateCode(testCode) shouldBe Some(1)
    }

    it should "process object with early definitions extending class with trait with refinement body definition with self with tts in everything" in new ToolboxTest {
      val testCode: String =
        s"""$ToolboxTestImports
           |
           |class Parent(val j: Int)
           |trait ParentTrait { def k: Int }
           |
           |tmsFor[Option[Int]]() {
           |  object TestObject extends {
           |    val e: Int = Some(2)
           |  } with Parent(Some(3)) with ParentTrait { self =>
           |    def k: Int = Some(4) + self.j + self.e
           |  }
           |  TestObject.k
           |}
           |
           |""".stripMargin

      evaluateCode(testCode) shouldBe Some(9)
    }

    it should "process class with companion object definition with tts in bodies" in new ToolboxTest {
      val testCode: String =
        s"""$ToolboxTestImports
           |
           |tmsFor[Option[Int]]$options {
           |  class TestClass(i: Int) {
           |    val j: Int = Some(1)
           |    def addToIJ(k: Int): Int = i + j + k
           |  }
           |  object TestClass {
           |    val o: Int = Some(2)
           |    def apply(): TestClass = new TestClass(o)
           |  }
           |
           |  val newTestClass = new TestClass(0)
           |  TestClass.o + TestClass().addToIJ(3) + newTestClass.addToIJ(4) + newTestClass.j
           |}
           |
           |""".stripMargin

      evaluateCode(testCode) shouldBe Some(14)
    }

    // *** Local Case Object creation

    it should "process case object without fields creation without tts" in new ToolboxTest {
      val testCode: String =
        s"""$ToolboxTestImports
           |
           |tmsFor[Option[AnyRef]]$options {
           |  Some(0) + 0 // stub to be tms
           |  case object TestObject
           |  TestObject
           |}
           |
           |""".stripMargin

      evaluateCode(testCode).asInstanceOf[Option[AnyRef]].get.getClass.getName should include ("TestObject$")
    }

    it should "process case object with refinement body creation with tts in refinement body" in new ToolboxTest {
      val testCode: String =
        s"""$ToolboxTestImports
           |
           |tmsFor[Option[Int]]$options {
           |  case object TestObject {
           |    val i: Int = Some(1)
           |  }
           |  TestObject.i
           |}
           |
           |""".stripMargin

      evaluateCode(testCode) shouldBe Some(1)
    }

    // *** Local Types & Local Generics

    it should "process typed class instantiation with tts" in new ToolboxTest {
      val testCode: String =
        s"""$ToolboxTestImports
           |
           |type T = Int
           |
           |tmsFor[Option[T]]$options {
           |  Some[T](1) + 1
           |}
           |
           |""".stripMargin

      evaluateCode(testCode) shouldBe Some(2)
    }

    it should "process type definition & class instantiation with tts" in new ToolboxTest {
      val testCode: String =
        s"""$ToolboxTestImports
           |
           |tmsFor[Option[Int]]$options {
           |  type T = Int
           |
           |  Some[T](Some(1) + 2)
           |}
           |
           |""".stripMargin

      evaluateCode(testCode) shouldBe Some(3)
    }

    it should "process generic case class default instantiation with tts: NON-TOOLBOX" in {
      {
        import sands.sugar.tms.TransparentMonads._

        case class TestClass[T](i: T)

        tmsFor[Option[Int]]() {
          TestClass[Int](Some(1)).i
        }
      } shouldBe Some(1)
    }

    it should "process generic case class instantiation with tts: NON-TOOLBOX" in {
      {
        import sands.sugar.tms.TransparentMonads._

        case class TestClass[T](i: T)(j: T)

        tmsFor[Option[Int]]() {
          new TestClass[Int](Some(5))(Some(6)).i
        }
      } shouldBe Some(5)
    }

    //2.11 toolbox: reflective compilation has failed: TestClass is not an enclosing class
    it should "process generic class instantiation with tts: NON-TOOLBOX" in {
      {
        import sands.sugar.tms.TransparentMonads._

        class TestClass[T](val i: T)

        tmsFor[Option[Int]]() {
          new TestClass[Int](Some(1)).i
        }
      } shouldBe Some(1)
    }

    if (ScalaStringVersion >= "2.12") {
      //2.11 toolbox & non-toolbox: reflective typecheck has failed: type mismatch;
      // found   : TestClass.this.i.type (with underlying type T)
      // required: T
      it should "process generic class definition & instantiation with tts" in new ToolboxTest {
        val testCode: String =
          s"""$ToolboxTestImports
             |
             |tmsFor[Option[Int]]$options {
             |  class TestClass[T](val i: T)
             |
             |  new TestClass[Int](Some(1)).i
             |}
             |
             |""".stripMargin

        evaluateCode(testCode) shouldBe Some(1)
      }
    }

    it should "process generic trait with field instantiation with tts" in new ToolboxTest {
      val testCode: String =
        s"""$ToolboxTestImports
           |
           |trait TestTrait[T] {
           |  val i: T
           |}
           |
           |tmsFor[Option[Int]]$options {
           |  new TestTrait[Int] {
           |    val i: Int = Some(3)
           |  }.i
           |}
           |
           |""".stripMargin

      evaluateCode(testCode) shouldBe Some(3)
    }

    it should "process generic trait with field definition & instantiation with tts" in new ToolboxTest {
      val testCode: String =
        s"""$ToolboxTestImports
           |
           |tmsFor[Option[Int]]$options {
           |  trait TestTrait[T] {
           |    val i: T
           |  }
           |  new TestTrait[Int] {
           |    val i: Int = Some(3)
           |  }.i
           |}
           |
           |""".stripMargin

      evaluateCode(testCode) shouldBe Some(3)
    }

    it should "process generic method calling with tts in the body" in new ToolboxTest {
      val testCode: String =
        s"""$ToolboxTestImports
           |
           |def test[T](v: T): T = v match {
           |  case i: Int => (i * 2).asInstanceOf[T]
           |  case s: String => (s * 2).asInstanceOf[T]
           |  case _ => v
           |}
           |
           |tmsFor[Option[String]]$options {
           |  test[Int](Some(3)).toString + test[String](Some("2")) + test[Long](Some(1L)).toString
           |}
           |
           |""".stripMargin

      evaluateCode(testCode) shouldBe Some("6221")
    }

    it should "process generic method definition & calling with tts in the body" in new ToolboxTest {
      val testCode: String =
        s"""$ToolboxTestImports
           |
           |tmsFor[Option[Int]]$options {
           |  def test[T](v: T): T = v match {
           |    case i: Int => (i * 2).asInstanceOf[T]
           |    case s: String => (s * 2).asInstanceOf[T]
           |    case _ => v
           |  }
           |  test[Int](Some(3))
           |}
           |
           |""".stripMargin

      evaluateCode(testCode) shouldBe Some(6)
    }
  }
}
