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

/*
 * Created by Serhiy Shamshetdinov
 * at 18.02.2021 17:13
 */

object TmsForTestOptions {
  def getBooleanSysPropOrEnvVal(key: String): Option[Boolean] =
    scala.sys.props.get(key).orElse(scala.sys.env.get(key)).map(_.equalsIgnoreCase("true"))

  val TestDebugEnabled: Boolean = getBooleanSysPropOrEnvVal("tmsTestDebug").getOrElse(false)
  val TestTraceEnabled: Boolean = getBooleanSysPropOrEnvVal("tmsTestTrace")getOrElse(TestDebugEnabled)

  val ToolboxReporterMinSeverity = 2 // 0 stands for INFO, 1 stands for WARNING and 2 stands for ERROR
  val ToolboxScalacOptions = "-unchecked -language:_" // tests are passed without this too

  val ForcedAllTestVariants: Boolean = true
  val TestForsStackVariants: Boolean = true
  val TestPreEvaluationVariants: Boolean = true

  val ForsStackForApplySourceOption: String = "Fors Stack For Apply Source"
  val NoForsStackForApplySourceOption: String = "No " + ForsStackForApplySourceOption
  val ForsStackForApplyParameterOption: String = "Fors Stack For Apply Parameter"
  val NoForsStackForApplyParameterOption: String = "No " + ForsStackForApplyParameterOption

  val ScalaStringVersion: String = util.Properties.versionNumberString
  val JavaStringVersion: String = util.Properties.javaVersion

  val CommonTestOptions: Seq[String] = (if (TestDebugEnabled) Seq("Debug") else Seq()) ++ (if (TestTraceEnabled) Seq("Trace") else Seq())
  val SimpleForsStackOptions: Seq[String] = CommonTestOptions ++ Seq("Single Fors Stack", "Pre Evaluate No Types")

  val ForsStackForApplySourceVariants: Seq[String] = Seq(NoForsStackForApplySourceOption, ForsStackForApplySourceOption)
  val ForsStackForApplyParameterVariants: Seq[String] = Seq(NoForsStackForApplyParameterOption, ForsStackForApplyParameterOption)
  val PreEvaluateVariants: Seq[String] = Seq("Pre Evaluate No Types", "Pre Evaluate All Types")

  def testWithForsStackWithPreEvaluationVariants(tests: Seq[String] => Unit): Unit =
    for {
      forsStackForApplySourceOption <- if (ForcedAllTestVariants || TestForsStackVariants) ForsStackForApplySourceVariants else ForsStackForApplySourceVariants.take(1)
      forsStackForApplyParameterOption <- if (ForcedAllTestVariants || TestForsStackVariants) ForsStackForApplyParameterVariants else ForsStackForApplyParameterVariants.take(1)
      preEvaluateOption <- if (ForcedAllTestVariants || TestPreEvaluationVariants) PreEvaluateVariants else PreEvaluateVariants.take(1)
    } {
      tests(CommonTestOptions ++ Seq(forsStackForApplySourceOption, forsStackForApplyParameterOption, preEvaluateOption))
    }

  def testWithSimpleOrForcedWithPreEvaluationVariants(tests: Seq[String] => Unit): Unit =
    if (ForcedAllTestVariants)
      testWithForsStackWithPreEvaluationVariants(tests)
    else
      tests(SimpleForsStackOptions)

  def testWithForsStackWithoutPreEvaluationVariants(tests: Seq[String] => Unit): Unit =
    for {
      forsStackForApplySourceOption <- if (ForcedAllTestVariants || TestForsStackVariants) ForsStackForApplySourceVariants else ForsStackForApplySourceVariants.take(1)
      forsStackForApplyParameterOption <- if (ForcedAllTestVariants || TestForsStackVariants) ForsStackForApplyParameterVariants else ForsStackForApplyParameterVariants.take(1)
    } {
      tests(CommonTestOptions ++ Seq(forsStackForApplySourceOption, forsStackForApplyParameterOption))
    }

  def testWithSimpleOrForcedWithoutPreEvaluationVariants(tests: Seq[String] => Unit): Unit =
    if (ForcedAllTestVariants)
      testWithForsStackWithoutPreEvaluationVariants(tests)
    else
      tests(SimpleForsStackOptions)
}
