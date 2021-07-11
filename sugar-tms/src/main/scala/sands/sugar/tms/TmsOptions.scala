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

import sands.sugar.tms.TmsOptions.{AllTypes, MonadicFlowType}
import sands.sugar.tms.TransparentMonads.tmsOptions

import scala.annotation.tailrec
import scala.collection.immutable.ListMap
import scala.reflect.macros.blackbox

/*
 * Created by Serhiy Shamshetdinov
 * at 16.12.2020 17:10
 */

private[tms] case class TmsOptions(debug: Boolean,
                                   trace: Boolean,
                                   embeddedForsCodeView: Boolean,
                                   predefCompliance: Boolean,
                                   forsStackForApplySource: Boolean,
                                   forsStackForApplyParameter: Boolean,
                                   preEvaluateTypes: Set[String],
                                   monadicFlowControl: Boolean) {
  val mfTypePreEvaluation: Boolean = preEvaluateTypes(MonadicFlowType) // when true: any type (of any Fors Stack) which conforms to auto detected Monadic Flow Base Type will be pre evaluated

  override def toString: String = "TmsOptions: " +
    s"debug=$debug, trace=$trace, monadicFlowControl=$monadicFlowControl, " +
    s"forsStackForApplySource=$forsStackForApplySource, forsStackForApplyParameter=$forsStackForApplyParameter, " +
    s"preEvaluateTypes=$preEvaluateTypes, predefCompliance=$predefCompliance, embeddedForsCodeView=$embeddedForsCodeView"

  def typeRequiresPreEvaluation(name1: String, name2: String): Boolean =
    preEvaluateTypes(AllTypes) || preEvaluateTypes(name1) || preEvaluateTypes(name2)

  val forceCodeView: Boolean = debug || embeddedForsCodeView
}

private[tms] object TmsOptions {
  val DefaultTmsOptions: TmsOptions = TmsOptions(
    debug = false,
    trace = false,
    embeddedForsCodeView = false, // when true: embeds private val into macro output with fors-code representation view
    predefCompliance = true, // when true: behavior complains to Predef any2stringadd: `Some(1) + "2"` results `"Some(1)2"`. When false: result is `Some("12")`
    forsStackForApplySource = false, // when true: does as early as possible evaluation of each apply part of expression
    forsStackForApplyParameter = false, // when true: does as early as possible evaluation of each parameter of expression
    preEvaluateTypes = Set(), // simple names or FQNs of types of the `for` to be pre-evaluated where possible (if `for` parameter expression is independent of the value with the same type)
    monadicFlowControl = false // when true: activates monadic flow for the root Block of input code
  )
  val DefaultMfOptions: TmsOptions = DefaultTmsOptions.copy(monadicFlowControl = true)

  private val PreEvaluate = "Pre Evaluate"
  private[tms] val AllTypes = "All Types"
  private[tms] val MonadicFlowType = "Monadic Flow Type"

  private lazy val ReadablePreEvaluateVariants = Set(PreEvaluate, normalizeOption(PreEvaluate).toUpperCase)
  private lazy val PreEvaluateLowerVariants = Set(PreEvaluate.toLowerCase, normalizeOption(PreEvaluate))

  val ForsStackForApplySourceOption: String = "Fors Stack For Apply Source"
  val NoForsStackForApplySourceOption: String = "No " + ForsStackForApplySourceOption
  val ForsStackForApplyParameterOption: String = "Fors Stack For Apply Parameter"
  val NoForsStackForApplyParameterOption: String = "No " + ForsStackForApplyParameterOption

  private lazy val TmsReadableOptionToSetter: Map[String, TmsOptions => TmsOptions] = ListMap(
    "No Debug" -> (_.copy(debug = false)),
    "Debug" -> (_.copy(debug = true)),

    "No Trace" -> (_.copy(trace = false)),
    "Trace" -> (_.copy(trace = true)),

    "No Predef Compliance" -> (_.copy(predefCompliance = false)),
    "Predef Compliance" -> (_.copy(predefCompliance = true)),

    NoForsStackForApplySourceOption -> (_.copy(forsStackForApplySource = false)),
    ForsStackForApplySourceOption -> (_.copy(forsStackForApplySource = true)),

    NoForsStackForApplyParameterOption -> (_.copy(forsStackForApplyParameter = false)),
    ForsStackForApplyParameterOption -> (_.copy(forsStackForApplyParameter = true)),

    "No Embedded Fors Code View" -> (_.copy(embeddedForsCodeView = false)),
    "Embedded Fors Code View" -> (_.copy(embeddedForsCodeView = true)),

    "Single Fors Stack" -> (_.copy(forsStackForApplySource = false, forsStackForApplyParameter = false)),
    "All Fors Stacks" -> (_.copy(forsStackForApplySource = true, forsStackForApplyParameter = true)),

    "Pre Evaluate No Types" -> (_.copy(preEvaluateTypes = Set())),
    "Pre Evaluate All Types" -> (_.copy(preEvaluateTypes = Set(AllTypes))),
    "Pre Evaluate Monadic Flow Type" -> (opt => opt.copy(preEvaluateTypes = opt.preEvaluateTypes + MonadicFlowType))
  )

  private[tms] lazy val AvailableTmsOptions: String =
    // don't "replace .map(_._1) with .keys" - it does not work correctly: it looses the order
    (TmsReadableOptionToSetter.map(_._1) ++ ReadablePreEvaluateVariants.map(_ + " <type-name>|<type-FQN>[, ...]"))
      .mkString("\"", "\", \"", "\"")

  private def normalizeOption(option: String): String = option.split("\\s").flatMap(_.headOption).mkString.toLowerCase

  private lazy val TmsReadableLowerOptionToSetter: Map[String, TmsOptions => TmsOptions] = TmsReadableOptionToSetter.map {
    case (key, value) => key.toLowerCase -> value
  }

  private lazy val TmsNormalizedOptionToSetter: Map[String, TmsOptions => TmsOptions] = TmsReadableOptionToSetter.map {
    case (key, value) => normalizeOption(key) -> value
  }

  def parseOptions(initialTmsOptions: TmsOptions, options: Seq[String], onError: String => Nothing): TmsOptions =
    options.flatMap {
      _.split(";").map(_.trim).filterNot(_.isEmpty)
    }.foldLeft(initialTmsOptions) {
      case (tmsOptions, option) => enrichWithParsedOption(tmsOptions, option, onError)
    }

  private def enrichWithParsedOption(tmsOptions: TmsOptions, option: String, onError: String => Nothing): TmsOptions = {
    val lowerCaseOption = option.toLowerCase
    TmsNormalizedOptionToSetter.get(lowerCaseOption).orElse(TmsReadableLowerOptionToSetter.get(lowerCaseOption))
      .map(_(tmsOptions))
      .orElse(
        PreEvaluateLowerVariants.find(pelv => lowerCaseOption.startsWith(pelv) && option.isDefinedAt(pelv.length) && option.charAt(pelv.length).isWhitespace).map { pev =>
          val typeNames = option.drop(pev.length).split(",").map(_.trim).toSet
          if (typeNames.isEmpty)
            onError(option)
          else
            tmsOptions.copy(preEvaluateTypes = tmsOptions.preEvaluateTypes ++ typeNames)
        }
      ).getOrElse(onError(option))
  }

  val DefaultTmsOptionsKey = "defaultTmsOptions"
  val OverrideTmsOptionsKey = "overrideTmsOptions"
}
