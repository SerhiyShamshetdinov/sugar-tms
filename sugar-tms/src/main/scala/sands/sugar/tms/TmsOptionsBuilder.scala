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
import sands.sugar.tms.TransparentMonads.tmsOptions

import scala.annotation.tailrec
import scala.reflect.macros.blackbox

/*
 * Created by Serhiy Shamshetdinov
 * at 23.06.2021 21:56
 */

trait TmsOptionsBuilder {
  val c: blackbox.Context
  import c.universe._

  def collectTmsOptions(defaultTmsOptions: TmsOptions, tmsLiteralOptions: Seq[c.Expr[String]]): TmsOptions = {
    val annotations = enclosingAnnotations(c.internal.enclosingOwner, Seq())
    val posToStringOptions =
      Seq(NoPosition -> getEnvAndSysPropOptions(DefaultTmsOptionsKey)) ++
        parseTmsAnnotations(annotations) ++
        Seq(c.enclosingPosition -> parseStringLiterals(tmsLiteralOptions),
          NoPosition -> getEnvAndSysPropOptions(OverrideTmsOptionsKey))

    posToStringOptions.foldLeft(defaultTmsOptions) {
      case (tmsOptions, (pos, stringOptions)) => parseOptions(tmsOptions, pos, stringOptions)
    }
  }

  def validateTypesToBePreEvaluated(stackTypes: Seq[Type])(implicit tmsOptions: TmsOptions): Unit = {
    val usedTypeNames = stackTypes.flatMap(tpe => Seq(tpe.typeSymbol.name.toString, tpe.typeSymbol.fullName))
    val nonStackTypeNames = tmsOptions.preEvaluateTypes -- Seq(AllTypes, MonadicFlowType) -- usedTypeNames
    if (nonStackTypeNames.nonEmpty) {
      c.warning(c.enclosingPosition, "the following TmsOptions pre evaluation type names are not found in the requested stack: " + nonStackTypeNames.mkString(", "))
    }
  }

  @tailrec
  private def enclosingAnnotations(symbol: Symbol, collectedAnnotations: Seq[Annotation]): Seq[Annotation] =
    if (symbol == rootMirror.RootClass)
      collectedAnnotations
    else
      enclosingAnnotations(symbol.owner, symbol.annotations ++ collectedAnnotations)

  private def parseTmsAnnotations(annotations: Seq[Annotation]): Seq[(Position, Seq[String])] =
    annotations.map(_.tree).collect {
      case tree@q"new ${annotationType: Type}(..${args: Seq[String]})" if annotationType == typeOf[tmsOptions] && args.nonEmpty =>
        (tree.pos, args)
    }

  private def parseStringLiterals(literals: Seq[c.Expr[String]]): Seq[String] =
    literals.map {
      _.tree match {
        case q"${string: String}" => string
        case other => abortParsing(c.enclosingPosition, other.toString)
      }
    }

  private def parseOptions(initialTmsOptions: TmsOptions, pos: Position, options: Seq[String]): TmsOptions =
    TmsOptions.parseOptions(initialTmsOptions, options, abortParsing(pos, _))

  private def abortParsing(pos: Position, opt: String): Nothing = {
    val toDescription = if (pos == NoPosition) s" to '$DefaultTmsOptionsKey' or '$OverrideTmsOptionsKey' system property" else ""
    c.abort(pos, s"Invalid tms option is passed$toDescription: '$opt'. Only the following string literals or ones abbreviations are supported (ignoring case): $AvailableTmsOptions")
  }

  def getEnvAndSysPropOptions(key: String): Seq[String] =
    Seq(scala.sys.env.get(key), scala.sys.props.get(key)).flatten
}
