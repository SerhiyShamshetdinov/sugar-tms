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

import scala.reflect.macros.blackbox
import scala.util.control.NonFatal

/*
 * Created by Serhiy Shamshetdinov
 * at 16.12.2020 17:13
 */

private[tms] trait TmsTools {
  val c: blackbox.Context
  import c.universe._

  // lazy val is also stable, so for lazy val symbol.asTerm.isStable = true
  def isLazyTerm(symbol: Symbol): Boolean = symbol.isTerm && symbol.asTerm.isLazy

  def typeConstructorConforms(tpe: Type, toTpe: Type): Boolean = {
    //  tpe.erasure <:< toTpe.erasure && !(tpe =:= typeOf[Nothing]) // the try to compare .erasure fails in Array HigherKinded type got from ttsN as type parameter (while other types work)
    tpe.typeConstructor <:< toTpe.typeConstructor && !(tpe =:= typeOf[Nothing]) // 'return' statement Type is Nothing
  }

  def innerOneParameterTypes(tpe: Type): List[Type] = {
    val types = innerDealiasedToOneParameterTypes(tpe)
    if (types.isEmpty)
      c.abort(c.enclosingPosition, "top of the tms types stack should contain at least one type that (possibly after dealiasing) has exactly one type parameter")
    else
      types
  }

  private def innerDealiasedToOneParameterTypes(tpe: Type): List[Type] = {
    val typeArgs = tpe.typeArgs
    if (typeArgs.length == 1)
      tpe :: innerDealiasedToOneParameterTypes(typeArgs.head)
    else {
      val dealiasedTpe = tpe.dealias
      if (tpe == dealiasedTpe)
        Nil
      else
        innerDealiasedToOneParameterTypes(dealiasedTpe)
    }
  }

  // .tpe type of input code tree `if (true) Some(1) else Some(2)` is Any.
  // `try 1` is also typechecked with Any when passed to macro.
  // Such types detection prevents normal flatMapping of extracted yield body (flattening the result).
  // possibly not required in Scala 3
  // The following helps (but 'match' sometimes still fail):
  def refineTreeType(tree: Tree): Type =
    if (tree.tpe =:= typeOf[Any]) {
      try {
        val newTree = c.parse(tree.toString)
        c.typecheck(newTree).tpe
      } catch {
        case NonFatal(th) =>
          c.warning(tree.pos, s"failed to refine type of the input code. Exception: ${th.getMessage}")
          tree.tpe
      }
    } else
      tree.tpe

  def collectTreeDefSymbols(tree: Tree): Set[Symbol] =
    tree.collect {
      case tree if tree != null && tree.isDef && tree.symbol != NoSymbol =>
        if (tree.symbol.isModule) {
          Seq(tree.symbol, tree.symbol.asModule.typeSignature.typeSymbol) // type Symbol of the module (of the object)
        } else {
          Seq(tree.symbol)
        }
    }.flatten.toSet

  def formattedTypeNames(types: Seq[Type]): String =
    types.map(_.typeSymbol.name).mkString("[", ", ", "]")

  def debug(s: => String)(implicit tmsOptions: TmsOptions): Unit =
    if (tmsOptions.debug) println(s"[debug] * tms $s")

  def trace(s: => String)(implicit tmsOptions: TmsOptions): Unit =
    if (tmsOptions.trace) println(s"[trace] * tms $s")

  def traceTree[E](prefix: => String)(expr: E)(implicit tmsOptions: TmsOptions): E = {
    trace(s"syntax tree: $prefix = $expr")
    expr
  }

  def traceExtraction[E](prefix: => String)(expr: E)(implicit tmsOptions: TmsOptions): E = {
    trace(s"fors stack extraction: $prefix = $expr")
    expr
  }

  def traceMonadicFlow[E](prefix: => String)(expr: E)(implicit tmsOptions: TmsOptions): E = {
    trace(s"Monadic Flow $prefix = $expr")
    expr
  }

  def debugExpr[E](prefix: => String, expr: c.Expr[E])(implicit tmsOptions: TmsOptions): c.Expr[E] = {
    if (tmsOptions.debug) {
      val pref = prefix
      debug(s"$pref code.tree: ${expr.tree}")
      try debug(s"$pref showCode(code.tree): ${showCode(expr.tree)}") // looks like the same as code.tree BUT actually is not the same and less buggy
      catch {
        case NonFatal(th: Throwable) => debug(s"$pref showCode(code.tree): failed with exception:\n${th.getMessage}")
        // showCode fails on class with CONSTRUCTOR having non-zero list of parameters: org/scala-lang/scala-reflect/2.12.12/scala-reflect-2.12.12-sources.jar!/scala/reflect/internal/ReificationSupport.scala:274
      }
      debug(s"$pref raw Expr: ${showRaw(expr, false)}")
    }
    expr
  }

  def showSymbol(symbol: Symbol): String = showRaw(symbol, printIds = true)

  def traceSymbolsSet(prefix: => String)(symbols: Set[Symbol])(implicit tmsOptions: TmsOptions): Set[Symbol] = {
    trace(s"$prefix: ${if (symbols.isEmpty) "none" else symbols.map(showSymbol).mkString(", ")}")
    symbols
  }

  private val shouldTraceTreeSymbols: Boolean = false
  private val shouldTraceEachSubtree: Boolean = true

  def traceTreeSymbols(suffix: => String)(tree: Tree)(implicit tmsOptions: TmsOptions): Tree = {
    if (shouldTraceTreeSymbols && tmsOptions.trace) {
      trace(s"Symbols of $suffix")
      tree.foreach { subtree =>
        if (shouldTraceEachSubtree || subtree.symbol != null && subtree.symbol != NoSymbol) {
          println(s"Subtree: $subtree\nSubtree raw: ${showRaw(subtree)}\nSubtree Symbol: ${showSymbol(subtree.symbol)}${if (subtree.symbol == null) "" else s"  Symbol Owner: ${showSymbol(subtree.symbol.owner)}"}\n")
        }
      }
    }
    tree
  }
}
