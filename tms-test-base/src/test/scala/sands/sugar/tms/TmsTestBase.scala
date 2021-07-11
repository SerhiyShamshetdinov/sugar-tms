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

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import sands.sugar.tms.TmsForTestOptions._

import scala.reflect.runtime.universe
import scala.reflect.{ClassTag, classTag}
import scala.tools.reflect.{ToolBox, mkConsoleFrontEnd}
import scala.util.Success

/*
 * Created by Serhiy Shamshetdinov
 * at 04.01.2021 19:21
 */

trait TmsTestBase extends AnyFlatSpec with Matchers {

  def literalOptions(options: Seq[String]): String =
    if (options.nonEmpty) options.mkString("(\"", "\", \"", "\")") else "()"

  val ToolboxTestImports: String =
    """import sands.sugar.tms.TransparentMonads._
      |import sands.sugar.tms.TmsImplicitsCommon.@@
      |
      |import scala.util.Try
      |import scala.reflect.classTag
      |
      |""".stripMargin

  val ToolboxTestImportsWithFuture: String =
    s"""$ToolboxTestImports
      |
      |import scala.concurrent.Future
      |
      |implicit object SameThreadExecutionContext extends scala.concurrent.ExecutionContext {
      |  override def execute(runnable: Runnable): Unit = runnable.run()
      |  override def reportFailure(cause: Throwable): Unit = scala.concurrent.ExecutionContext.defaultReporter(cause)
      |}
      |
      |""".stripMargin

  trait ToolboxTest {
    val toolbox: ToolBox[universe.type] = reflect.runtime.currentMirror.mkToolBox(mkConsoleFrontEnd(ToolboxReporterMinSeverity), ToolboxScalacOptions)
    import toolbox.u._

    def parseAndLogTrees(code: String): Tree = {
      val tree = toolbox.parse(code)
      if (TestDebugEnabled) println(s"Test input code:\n$tree")

      val typeCheckedTree = toolbox.typecheck(tree)
      if (TestDebugEnabled) println(s"Test type checked output code:\n$typeCheckedTree")

      toolbox.untypecheck(typeCheckedTree)
    }

    def evaluateTree(tree: Tree): Any = toolbox.compile(tree)()

    def evaluateCode(code: String): Any = evaluateTree(parseAndLogTrees(code))

    def evaluatedCode(code: String): (String, Any) = {
      val typecheckedTree = parseAndLogTrees(code)
      (typecheckedTree.toString, evaluateTree(typecheckedTree))
    }
  }

  def countSubstrings(string: String, regex: String): Int = string.split(regex).length - 1

  def countVals(code: String): Int = countSubstrings(code, "val ")

  def validateForsStacksNumber(code: String,
                               tmsOptions: Seq[String],
                               noArgForsStackNoParamForsStack: Int,
                               noArgForsStackYesParamForsStack: Int,
                               yesArgForsStackNoParamForsStack: Int,
                               yesArgForsStackYesParamForsStack: Int): Unit = {
    val forsNumber = countSubstrings(code, "\\.map\\[")
    val expectedForsNumber = if (!tmsOptions.exists(ForsStackForApplySourceVariants.contains) || !tmsOptions.exists(ForsStackForApplyParameterVariants.contains)) {
      fail(s"Test requires tmsOptions contains one of $ForsStackForApplySourceVariants option AND one of $ForsStackForApplyParameterVariants option but test was run with options $tmsOptions")
    } else {
      (tmsOptions.contains[String](ForsStackForApplySourceOption), tmsOptions.contains[String](ForsStackForApplyParameterOption)) match {
        case (false, false) => noArgForsStackNoParamForsStack
        case (false, true) => noArgForsStackYesParamForsStack
        case (true, false) => yesArgForsStackNoParamForsStack
        case (true, true) => yesArgForsStackYesParamForsStack
      }
    }

    assert(forsNumber == expectedForsNumber, "expected number of Fors Stacks")
  }

  case class StackTestData(accessor: String, aggregator: String, result: Any, resultType: String, implicitConversionShouldExist: Boolean = true)

  val MaxTtsNumber = 5

  def implicitExtensionOnTtsStacksTests(options: String,
                                        testValue: String,
                                        testValueType: String,
                                        typecheckedImplicitMethod: String,
                                        implicitType: String,
                                        data: StackTestData *): Unit =
    data.foreach {
      case StackTestData(accessor, aggregator, result, resultType, implicitConversionShouldExist) =>

        val conversionType = if (implicitType.nonEmpty)
          s"${if (implicitConversionShouldExist) "by" else "without"} implicit $typecheckedImplicitMethod conversion to $implicitType"
        else "directly"

        it should s"reach $testValueType $accessor accessor $conversionType" in new ToolboxTest {
          val testCode: String =
            s"""$ToolboxTestImports
               |
               |tmsFor[Try[$resultType]]$options {
               |  Try($testValue)$accessor $aggregator
               |  Try(Try($testValue))$accessor $aggregator
               |  Try(Try(Try($testValue)))$accessor $aggregator
               |  Try(Try(Try(Try($testValue))))$accessor $aggregator
               |  Try(Try(Try(Try(Try($testValue)))))$accessor
               |}
               |""".stripMargin

          private val tree = parseAndLogTrees(testCode)

          if (typecheckedImplicitMethod.nonEmpty) {
            if (implicitConversionShouldExist)
              tree.toString should include(typecheckedImplicitMethod)
            else
              tree.toString shouldNot include(typecheckedImplicitMethod)
          }

          evaluateTree(tree) should matchPattern {
            case gotResult if TestDebugEnabled && { println(s"gotResult=$gotResult expectedInner=$result"); false } =>
            case Success(res) if res == result =>
          }
        }
    }

  def implicitPrimitivesConversionTest[ST: ClassTag, RT: ClassTag](options: String, helper: String, testValue: String, conversionResult: String): Unit = {

    def className[CT: ClassTag]: String = {
      val name = classTag[CT].runtimeClass.getName
      if (name.startsWith("java.")) name else name.capitalize
    }

    val sourceTypeName = className[ST]
    val resultTypeName = className[RT]

    it should s"implicitly convert $sourceTypeName value to $resultTypeName value by $helper via tts1-tts5 overloads" in new ToolboxTest {

      val testCode: String =
        s"""$ToolboxTestImports
           |
           |tmsFor[Option[String]]$options(
           |  identity[$resultTypeName](Some($testValue: $sourceTypeName)).toString +
           |  identity[$resultTypeName](Some(Some($testValue: $sourceTypeName))).toString +
           |  identity[$resultTypeName](Some(Some(Some($testValue: $sourceTypeName)))).toString +
           |  identity[$resultTypeName](Some(Some(Some(Some($testValue: $sourceTypeName))))).toString +
           |  identity[$resultTypeName](Some(Some(Some(Some(Some($testValue: $sourceTypeName)))))).toString
           |)
           |""".stripMargin

      private val tree = parseAndLogTrees(testCode)

      tree.toString should include (helper)

      val expectedInner: String = conversionResult * MaxTtsNumber
      evaluateTree(tree) should matchPattern {
        case result if TestDebugEnabled && { println(s"result=$result expectedInner=$expectedInner"); false } =>
        case Some(result) if result == expectedInner =>
      }
    }
  }
}
