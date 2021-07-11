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

/*
 * Created by Serhiy Shamshetdinov
 * at 08.12.2020 13:47
 */

private[tms] class TmsTransformations[C <: blackbox.Context](val c: C)(implicit val tmsOptions: TmsOptions) extends TmsTree {
  import c.universe._

  def unliftTmsTreeExpression(tmsCodeTree: Tree): TmsExpression =
    traceTreeSymbols("INPUT tree")(tmsCodeTree) match {
      case q"${tmsExpression: TmsExpression}" => tmsExpression
      case _ => c.abort(c.enclosingPosition, "macros failed to unlift TmsExpression from the input tmsCode")
    }

  def preprocessTmsExpression(tmsExpression: TmsExpression): TmsExpression =
    tmsExpression.preprocess

  def buildExtractionContext(tmsExpression: TmsExpression, forsStackTypes: List[Type], inputDefSymbols: Set[Symbol]): TmsExtractionContext =
    if (tmsOptions.monadicFlowControl)
      MonadicFlowControl.buildExtractionContext(tmsExpression, forsStackTypes, inputDefSymbols)
    else
      TmsExtractionContext.withoutMonadicFlow(inputDefSymbols)

  def extractForsStack(tmsExpression: TmsExpression, forsStackTypes: List[Type])(implicit context: TmsExtractionContext): TmsExtractedForsStack = {
    val tmsForsBuilder = TmsForsBuilder(forsStackTypes)
    val tmsForExtract = if (tmsOptions.monadicFlowControl)
      MonadicFlowControl.extractRootBlock(tmsExpression, tmsForsBuilder)
    else
      tmsExpression.extract(tmsForsBuilder)

    TmsExtractedForsStack(tmsForExtract.forsBuilder, tmsForExtract.extract)
  }

  def postprocessExtractedForsStack(rootExtractedForsStack: TmsExtractedForsStack, tmsCodeTree: Tree)(implicit context: TmsExtractionContext): TmsExtractedForsStack =
    TmsExtractedForsStack.postprocessRootExtractedForsStack(rootExtractedForsStack, refineTreeType(tmsCodeTree))

  def liftForsStackTree(tmsExtractedForsStack: TmsExtractedForsStack, tmsCodeDefSymbols: Set[Symbol]): Tree = {
    val liftedTree = tmsExtractedForsStack.forsBuilder.liftForsStack(tmsExtractedForsStack.yieldTree)

    new UntypecheckTransformer(tmsCodeDefSymbols).transform(liftedTree)
  }

  def withOptionalEmbeddedForsCodeView(forsCodeView: String, forsStackTree: Tree)(implicit tmsOptions: TmsOptions): Tree =
    if (tmsOptions.embeddedForsCodeView)
      q"val ${c.freshName(TermName("forsCodeView$"))} = ${s"\n$forsCodeView\n"}; $forsStackTree"
    else
      forsStackTree


  class UntypecheckTransformer(tmsCodeDefSymbols: Set[Symbol]) extends Transformer {

    override def transform(tree: Tree): Tree = tree match {
      case tree@Ident(name: Name) if tmsCodeDefSymbols(tree.symbol) =>
        traceTree(s"Replacing Ident with local definition ref Symbol '${tree.symbol}' by new one without ref Symbol")(showRaw(tree))
        Ident(name)

      case tree@Select(qualifier: Tree, name: Name) if tmsCodeDefSymbols(tree.symbol) =>
        traceTree(s"Replacing Select with local definition ref Symbol '${tree.symbol}' by new one without ref Symbol")(showRaw(tree))
        Select(transform(qualifier), name)

      case tree@This(typeName: TypeName) if tmsCodeDefSymbols(tree.symbol) =>
        traceTree(s"Replacing This with local definition ref Symbol '${tree.symbol}' by new one without ref Symbol")(showRaw(tree))
        This(typeName)

      case tree: TypeDef => // TypeDefs were not untypechecked while lifting
        traceTree("Untypechecking TypeDef")(showRaw(tree))
        super.transform(c.untypecheck(tree))

      case tree@CaseDef(pattern, guard, body) => // CaseDef patterns were not untypechecked while lifting (for validation of proper def usages while postprocessing)
        traceTree("Untypechecking CaseDef")(showRaw(tree))
        super.transform(CaseDef(c.untypecheck(pattern), guard, body))

      case tree if tree.isDef => // resets def symbols restored to lifted def trees for validation of proper def usages while postprocessing
        traceTree("Setting NoSymbol to definition tree")(showRaw(tree))
        c.internal.setSymbol(tree, NoSymbol)
        super.transform(tree)

      case _ =>
        super.transform(tree)
    }
  }
}
