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

import sands.sugar.tms.TmsOptions._
import sands.sugar.tms.TransparentMonads.{fromScalaVersionIncluding, toScalaVersionExcluding}

import scala.reflect.macros.blackbox

/*
 * Created by Serhiy Shamshetdinov
 * at 03.12.2020 14:28
 */

private[tms] class TransparentMonadsImpl(val c: blackbox.Context) extends TmsOptionsBuilder {
  import c.universe._

  protected[tms] def defaultTmsOptions: TmsOptions = DefaultTmsOptions

  /**
   * @tmsCode type is Any because it may be any inner type of O types stack after applying ttsNxxx implicits to it
   */
  def transparentMonadsImpl[O: c.WeakTypeTag](tmsCode: c.Expr[Any]): c.Expr[O] = transparentMonadsForImpl[O](Nil: _*)(tmsCode)

  def transparentMonadsForImpl[O: c.WeakTypeTag](tmsLiteralOptions: c.Expr[String] *)(tmsCode: c.Expr[Any]): c.Expr[O] = {

    val scalaVersionNumber = util.Properties.versionNumberString
    val scalaSemanticVersion: SemanticVersion = SemanticVersion(scalaVersionNumber)
    if (fromScalaVersionIncluding.exists(scalaSemanticVersion < _) || toScalaVersionExcluding.exists(scalaSemanticVersion >= _))
      c.abort(c.enclosingPosition, s"Current macro code is compiled for usage with Scala version ${fromScalaVersionIncluding.fold("")(v => s"from $v ")}" +
        s"${toScalaVersionExcluding.fold("")(v => s"up to $v excluding")}. Please see the documentation on how to add artifact dependency for your Scala version $scalaVersionNumber")

    implicit val tmsOptions: TmsOptions = collectTmsOptions(defaultTmsOptions, tmsLiteralOptions)

    val tmsTransformations: TmsTransformations[c.type] = new TmsTransformations[c.type](c)
    import tmsTransformations.{c => _, _}

    if (tmsOptions.debug || tmsOptions.trace) {
      c.info(c.enclosingPosition, "\n* tms >>> debug/trace of macro processing", force = true)
    }
    debug(tmsOptions.toString)
    debugExpr("INPUT", tmsCode)

    val forsStackTypes = innerOneParameterTypes(weakTypeTag[O].tpe)
    validateTypesToBePreEvaluated(forsStackTypes)

    val tmsCodeTree: Tree = tmsCode.tree

    val unliftedTmsExpression: TmsExpression = unliftTmsTreeExpression(tmsCodeTree)

    val outputExpr: c.Expr[O] = if (!tmsOptions.monadicFlowControl && !unliftedTmsExpression.hasTts) {
      c.warning(c.enclosingPosition, "no tts operations (reachable by TmsTree AST) are found. Macros input code is returned AS IS without transformations")
      c.Expr[O](tmsCodeTree)
    } else {
      val preprocessedTmsExpression: TmsExpression = preprocessTmsExpression(unliftedTmsExpression)

      val inputDefSymbols = traceSymbolsSet("input code definition Symbols")(collectTreeDefSymbols(tmsCodeTree))

      implicit val context: TmsExtractionContext = buildExtractionContext(preprocessedTmsExpression, forsStackTypes, inputDefSymbols)

      val extractedForsStack: TmsExtractedForsStack = extractForsStack(preprocessedTmsExpression, forsStackTypes)
      val extractedForsStackCodeView: String = if (tmsOptions.forceCodeView) extractedForsStack.codeView else ""
      debug("extracted fors code view:\n" + extractedForsStackCodeView)

      val postprocessedTmsForsStack: TmsExtractedForsStack = postprocessExtractedForsStack(extractedForsStack, tmsCodeTree)
      if (postprocessedTmsForsStack.forsBuilder.usedTypes.isEmpty) {
        c.warning(c.enclosingPosition, "there is no built fors in the macros transformation result. Applying macros to passed code has no sense")
      }
      val postprocessedForsStackCodeView: String = if (tmsOptions.forceCodeView) postprocessedTmsForsStack.codeView else ""
      if (tmsOptions.debug && extractedForsStackCodeView == postprocessedForsStackCodeView)
        debug("postprocessed fors are unchanged")
      else
        debug("postprocessed fors code view:\n" + postprocessedForsStackCodeView)

      val forsStackTree: Tree = liftForsStackTree(postprocessedTmsForsStack, inputDefSymbols)

      debugExpr("OUTPUT", c.Expr[O](withOptionalEmbeddedForsCodeView(postprocessedForsStackCodeView, forsStackTree)))
    }

    traceTreeSymbols("OUTPUT tree")(outputExpr.tree)
    outputExpr
  }

}

