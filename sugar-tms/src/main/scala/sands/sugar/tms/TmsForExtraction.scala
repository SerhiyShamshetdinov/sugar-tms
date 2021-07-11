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

import scala.annotation.tailrec

/*
 * Created by Serhiy Shamshetdinov
 * at 16.12.2020 13:14
 */

/** TmsForExtractor is much like standard macro Lifting but it accepts & returns TmsForsBuilder. TmsForExtractor does `for`s extraction
 * by replacing parts of TmsTree to for-collected value names & Lifts other code as scala Tree. 
 *
 * TmsForExtractor takes the node of TmsTree and returns corresponding tree in terms of TmsExtracted (Scala Tree or Fors Stack).
 * It moves ttsNxxx argument expressions to forsBuilder and replaces this ttsNxxx call by "extracted" for value name.
 */
private[tms] trait TmsForExtraction extends TmsExtractedPostprocessing with TmsCodeViewing { _: TmsTree =>
  import c.universe._

  case class TmsExtractionContext(mfBaseType: Type, mfForCollectorIndex: Int, mfValDefSymbols: Set[Symbol], inputDefSymbols: Set[Symbol]) {

    val nonMfValDefSymbols: Set[Symbol] = inputDefSymbols -- mfValDefSymbols

    private val NameToIdent = mfValDefSymbols.map { symbol =>
      val symbolTermName = symbol.asTerm.name
      symbolTermName.toString -> c.internal.setSymbol(Ident(symbolTermName), symbol)
    }.toMap.withDefault(name => Ident(TermName(name)))

    def identWithMfSymbol(termName: TermName): Ident = NameToIdent(termName.toString)

    def isMfVal(termName: TermName): Boolean = NameToIdent.isDefinedAt(termName.toString)
  }

  object TmsExtractionContext {
    def withoutMonadicFlow(codeDefSymbols: Set[Symbol], mfType: Type = NoType): TmsExtractionContext =
      new TmsExtractionContext(mfType, -1, Set(), codeDefSymbols)
  }

  case class TmsForExtract[O](forsBuilder: TmsForsBuilder, extract: O)

  trait TmsForExtractor[S, O] {
    /** should not be used explicitly on extractor: use extractWithMaxOfReservedTypes or TmsForExtractorSyntax */
    def extract(source: S, tmsForsBuilder: TmsForsBuilder)(implicit context: TmsExtractionContext): TmsForExtract[O]
  }

  def extractWithMaxOfReservedTypes[T, O](extractor: TmsForExtractor[T, O], source: T, tmsForsBuilder: TmsForsBuilder)(implicit context: TmsExtractionContext): TmsForExtract[O] = {
    val tmsForExtract = extractor.extract(source, tmsForsBuilder)
    TmsForExtract[O](tmsForExtract.forsBuilder.withMaxOfReservedTypes(tmsForsBuilder), tmsForExtract.extract)
  }

  implicit class TmsForExtractorSyntax[T, O](source: T)(implicit extractor: TmsForExtractor[T, O]) {
    def extract(tmsForsBuilder: TmsForsBuilder)(implicit context: TmsExtractionContext): TmsForExtract[O] =
      extractWithMaxOfReservedTypes(extractor, source, tmsForsBuilder)
  }

  object TmsForExtractor {

    implicit def seqForExtractor[T, O](implicit lemExtractor: TmsForExtractor[T, O]): TmsForExtractor[Seq[T], Seq[O]] = new TmsForExtractor[Seq[T], Seq[O]] {
      def extract(source: Seq[T], tmsForsBuilder: TmsForsBuilder)(implicit context: TmsExtractionContext): TmsForExtract[Seq[O]] = {
        source.foldLeft(TmsForExtract(tmsForsBuilder, Seq.empty[O])) {
          case (TmsForExtract(forsBuilder, builtTrees), sourceLem) =>
            val lemTmsForExtract = extractWithMaxOfReservedTypes(lemExtractor, sourceLem, forsBuilder)
            TmsForExtract(lemTmsForExtract.forsBuilder, builtTrees :+ lemTmsForExtract.extract)
        }
      }
    }
  }

  def treeDependsOnIdent(ident: Ident, tree: Tree): Boolean = {
    val IdentName = ident.name
    tree.exists {
      case ti@Ident(IdentName) => ti.symbol == ident.symbol // NoSymbol for unique generated for enum termNames or real symbol for Monadic Flow vals
      case _ => false
    }
  }

  sealed trait TmsExtracted {
    def defSymbols: Set[Symbol]

    def findRefSymbolOf(defSymbols: Set[Symbol]): Option[(Symbol, Position)]

    def dependsOnIdent(ident: Ident): Boolean

    def isValRefTree: Boolean = this match {
      case TmsExtractedTree(tree: RefTree) => // ref Idents & [nested] Selects of ones
        val symbol = tree.symbol
        symbol.isTerm && symbol.asTerm.isStable || symbol == NoSymbol && tree.isTerm // last is for internal `for` Idents with NoSymbol created while extraction
      case _ =>
        false
    }

    def isNonLazyValRefTree: Boolean =
      isValRefTree && !this.asInstanceOf[TmsExtractedTree].tree.symbol.asTerm.isLazy // .asInstanceOf[TmsExtractedTree] is Ok here when isValRefTree=true

    def equalsByRefTree(otherTmsExtracted: TmsExtracted): Boolean =
      (this, otherTmsExtracted) match {
        case (TmsExtractedTree(tree1@Ident(termName1: TermName)), TmsExtractedTree(tree2@Ident(termName2: TermName)))
          if tree1.symbol == NoSymbol && tree2.symbol == NoSymbol => // case for generated `for` inner vals of <- : untypechecked constructed Idents with NoSymbols, may not be the Select, only Idents
          termName1 == termName2
        case (TmsExtractedTree(tree1: RefTree), TmsExtractedTree(tree2: RefTree)) => // case for initially typechecked Idents & [nested] Selects with typechecked Idents & monadic flow Idents (with Symbols added while construction)
//          traceExtraction(s"equalsByTermTree: Enum tree1.symbol=${showSymbol(tree1.symbol)}, Other tree2.symbol=${showSymbol(tree2.symbol)}")("")
          tree1.symbol != NoSymbol && tree1.symbol == tree2.symbol
        case _ =>
          false
      }
  }

  object TmsExtracted {

    implicit val tmsExtractedLiftable: Liftable[TmsExtracted] = Liftable[TmsExtracted] {
      case extractedTree: TmsExtractedTree => q"$extractedTree"
      case extractedForsStack: TmsExtractedForsStack => q"$extractedForsStack"
    }

    implicit val tmsExtractedCodeViewer: TmsCodeViewer[TmsExtracted] = new TmsCodeViewer[TmsExtracted] {
      def codeView(source: TmsExtracted): String = source match {
        case extractedTree: TmsExtractedTree => extractedTree.codeView
        case extractedForsStack: TmsExtractedForsStack => extractedForsStack.codeView
      }
    }

    implicit val tmsExtractedPostprocessor: TmsExtractedPostprocessor[TmsExtracted] = new TmsExtractedPostprocessor[TmsExtracted] {
      def postprocess(source: TmsExtracted)(implicit context: TmsExtractionContext): TmsExtracted = source match {
        case extractedTree: TmsExtractedTree => extractedTree.postprocess
        case extractedForsStack: TmsExtractedForsStack => extractedForsStack.postprocess
      }
    }
  }

  case class TmsExtractedTree(tree: Tree) extends TmsExtracted {
    lazy val defSymbols: Set[Symbol] = collectTreeDefSymbols(tree)

    def findRefSymbolOf(defSymbols: Set[Symbol]): Option[(Symbol, Position)] =
      tree.find(subtree => !subtree.isDef && defSymbols(subtree.symbol)).map(st => st.symbol -> st.pos)

    def dependsOnIdent(ident: Ident): Boolean =
      treeDependsOnIdent(ident, tree)
  }

  object TmsExtractedTree {

    implicit val tmsExtractedTreeLiftable: Liftable[TmsExtractedTree] = Liftable[TmsExtractedTree] {
      _.tree
    }

    implicit val tmsExtractedTreeCodeViewer: TmsCodeViewer[TmsExtractedTree] = new TmsCodeViewer[TmsExtractedTree] {
      def codeView(source: TmsExtractedTree): String = source.tree.codeView
    }

    implicit val tmsExtractedTreePostprocessor: TmsExtractedPostprocessor[TmsExtractedTree] = new TmsExtractedPostprocessor[TmsExtractedTree] {
      def postprocess(source: TmsExtractedTree)(implicit context: TmsExtractionContext): TmsExtractedTree = source
    }
  }

  case class TmsExtractedForsStack(forsBuilder: TmsForsBuilder, yieldTree: Tree) extends TmsExtracted {
    lazy val defSymbols: Set[Symbol] = collectTreeDefSymbols(yieldTree) ++ forsBuilder.defSymbols

    def findRefSymbolOf(defSymbols: Set[Symbol]): Option[(Symbol, Position)] =
      TmsExtractedTree(yieldTree).findRefSymbolOf(defSymbols) orElse
        forsBuilder.findRefSymbolOf(defSymbols)

    def dependsOnIdent(ident: Ident): Boolean =
      dependsOnIdent(ident, 0)

    def dependsOnIdent(ident: Ident, fromCollectorIndex: Int): Boolean =
      treeDependsOnIdent(ident, yieldTree) || forsBuilder.forCollectors.drop(fromCollectorIndex).exists(_.dependsOnIdent(ident))
  }

  object TmsExtractedForsStack {

    implicit val tmsExtractedForsStackLiftable: Liftable[TmsExtractedForsStack] = Liftable[TmsExtractedForsStack] { source =>
      source.forsBuilder.liftForsStack(source.yieldTree)
    }

    implicit val tmsExtractedForsStackCodeViewer: TmsCodeViewer[TmsExtractedForsStack] = new TmsCodeViewer[TmsExtractedForsStack] {
      def codeView(source: TmsExtractedForsStack): String = {
        val yieldTreeCodeView = source.yieldTree match {
          case _: Block => source.yieldTree.codeView
          case _ => block(source.yieldTree.codeView)
        }
        source.forsBuilder.forCollectors.filter(_.nonEmpty)
          .foldRight(yieldTreeCodeView) {
            case (forCollector, body) => block(forCollector.codeView + body)
          }
      }
    }

    implicit val tmsExtractedForsStackPostprocessor: TmsExtractedPostprocessor[TmsExtractedForsStack] = new TmsExtractedPostprocessor[TmsExtractedForsStack] {
      def postprocess(source: TmsExtractedForsStack)(implicit context: TmsExtractionContext): TmsExtractedForsStack = {
        val postprocessedForsStack = source.copy(
          forsBuilder = source.forsBuilder.postprocess,
          yieldTree = TmsExtractedTree(source.yieldTree).postprocess.tree
        )

        dropDegenerateForCollectors(postprocessedForsStack)
      }
    }

    /** Root TmsExtractedForsStack postprocessing differs from postprocessing of inner TmsExtractedForsStacks (defined above by implicit tmsExtractedForsStackPostprocessor) in:
     *  - if root Fors Stack is Monadic Flow (other Fors Stacks may not be Monadic Flow) it tries to inline non used directly MFC val's enum pair 'mfVal = mfInit' (or single PreEval) + 'mfValue <- mfVal' to single 'mfValue <- mfInit'
     *  - macro tries to flatMap yield body only for the root Fors Stack
     *  - root Fors Stack yield body is not postprocessed
     */
    def postprocessRootExtractedForsStack(rootForsStack: TmsExtractedForsStack, rootForYieldType: Type)(implicit context: TmsExtractionContext): TmsExtractedForsStack = {
      val withPostprocessedForsBuilder = rootForsStack.copy(forsBuilder = rootForsStack.forsBuilder.postprocess) // also postprocesses inner Fors Stacks if any
      val withPostprocessedMonadicFlow = MonadicFlowControl.postprocessExtractedForsStack(withPostprocessedForsBuilder)
      val withoutDegenerateFors = dropDegenerateForCollectors(withPostprocessedMonadicFlow)

      flatMapYieldBodyOfExtractedForsStack(withoutDegenerateFors, rootForYieldType)
    }

    // Yield body of the only root extracted Fors Stack may be flatMapped since this operation changes result type of the whole Fors Stack &
    // thus transformation result type may depend on the optional "Fors Stack For Apply Source" or "Fors Stack For Apply Parameter" modes
    // (having one these options on all Fors Stacks should do (or not do) flatMapping at the same type but this is impossible due to different expressions of each inner Fors Stack)
    private def flatMapYieldBodyOfExtractedForsStack(rootExtractedForsStack: TmsExtractedForsStack, rootForYieldType: Type)(implicit context: TmsExtractionContext): TmsExtractedForsStack = {
      val flatMappableCollectorIndex = rootExtractedForsStack.forsBuilder.flatMappableCollectorIndex(rootForYieldType)
      if (flatMappableCollectorIndex == -1)
        rootExtractedForsStack
      else {
        val yieldExtractedTree = TmsExtractedTree(rootExtractedForsStack.yieldTree)
        rootExtractedForsStack.forsBuilder.forCollectors(flatMappableCollectorIndex).findReusableExtractedIdent(yieldExtractedTree)
          .map { reusableIndent =>
            rootExtractedForsStack.copy(yieldTree = reusableIndent)
          }
          .getOrElse {
            val flatMappedTermName = c.freshName(TermName(s"flatMappedValueOf${rootForYieldType.typeSymbol.name.toString}$$"))
            val flatMappedForArrowEnum = TmsForArrowEnum(flatMappedTermName, yieldExtractedTree)
            TmsExtractedForsStack(
              forsBuilder = rootExtractedForsStack.forsBuilder.withForEnum(flatMappableCollectorIndex, flatMappedForArrowEnum),
              yieldTree = Ident(flatMappedTermName)
            )
          }
      }
    }

    @tailrec
    private def dropDegenerateForCollectors(tmsExtractedForsStack: TmsExtractedForsStack): TmsExtractedForsStack = {
      val degenerateForCollectorIndex = tmsExtractedForsStack.forsBuilder.getDegenerateForCollectorIndex(tmsExtractedForsStack.yieldTree)
      if (degenerateForCollectorIndex == -1)
        tmsExtractedForsStack
      else {
        val degenerateForCollector = tmsExtractedForsStack.forsBuilder.forCollectors(degenerateForCollectorIndex)
        val replaceToTmsExtracted = if (degenerateForCollector.forPreEvals.nonEmpty)
          degenerateForCollector.forPreEvals.head.tmsExtracted
        else
          degenerateForCollector.forEnums.head.tmsExtracted

        replaceToTmsExtracted match { // this check for type matching just in case : inner `for` in TmsExtractedForsStack may not happen here (as to TmsExtracted Tree construction, looks like that :) )
          case TmsExtractedTree(tree) =>
            val withDroppedDegenerate = TmsExtractedForsStack(
              forsBuilder = tmsExtractedForsStack.forsBuilder.withUpdatedForCollector(degenerateForCollectorIndex, _.reset),
              yieldTree = tree
            )
            dropDegenerateForCollectors(withDroppedDegenerate)

          case _ =>
            c.warning(c.enclosingPosition, "tms optimization is not full: TmsExtractedForsStack does happen here and it is returned non-optimized")
            tmsExtractedForsStack
        }
      }
    }
  }

  sealed trait TmsForEnum {
    def termName: TermName
    def tmsExtracted: TmsExtracted
  }

  case class TmsForArrowEnum(termName: TermName, tmsExtracted: TmsExtracted) extends TmsForEnum
  case class TmsForAssignEnum(termName: TermName, tmsExtracted: TmsExtracted) extends TmsForEnum

  object TmsForEnum {

    implicit val tmsForEnumLiftable: Liftable[TmsForEnum] = Liftable[TmsForEnum] {
      case TmsForArrowEnum(termName, tmsExtracted) => fq"$termName <- $tmsExtracted"
      case TmsForAssignEnum(termName, tmsExtracted) => fq"$termName = $tmsExtracted"
    }

    implicit val tmsForEnumCodeViewer: TmsCodeViewer[TmsForEnum] = new TmsCodeViewer[TmsForEnum] {
      def codeView(source: TmsForEnum): String = source match {
        case TmsForArrowEnum(termName, tmsExtracted) => s"$termName <- ${tmsExtracted.codeView}"
        case TmsForAssignEnum(termName, tmsExtracted) => s"$termName = ${tmsExtracted.codeView}"
      }
    }

    implicit val tmsForEnumPostprocessor: TmsExtractedPostprocessor[TmsForEnum] = new TmsExtractedPostprocessor[TmsForEnum] {
      def postprocess(source: TmsForEnum)(implicit context: TmsExtractionContext): TmsForEnum = source match {
        case arrowEnum: TmsForArrowEnum => arrowEnum.copy(tmsExtracted = arrowEnum.tmsExtracted.postprocess)
        case assignEnum: TmsForAssignEnum => assignEnum.copy(tmsExtracted = assignEnum.tmsExtracted.postprocess)
      }
    }
  }

  case class TmsForPreEvaluationVal(termName: TermName, tmsExtracted: TmsExtracted)

  object TmsForPreEvaluationVal {

    implicit val tmsForPreEvaluationValLiftable: Liftable[TmsForPreEvaluationVal] = Liftable[TmsForPreEvaluationVal] {
      source => q"val ${source.termName} = ${source.tmsExtracted};"
    }

    implicit val tmsForPreEvaluationValCodeViewer: TmsCodeViewer[TmsForPreEvaluationVal] = new TmsCodeViewer[TmsForPreEvaluationVal] {
      def codeView(source: TmsForPreEvaluationVal): String = s"val ${source.termName} = ${source.tmsExtracted.codeView}"
    }

    implicit val tmsForPreEvaluationValPostprocessor: TmsExtractedPostprocessor[TmsForPreEvaluationVal] = new TmsExtractedPostprocessor[TmsForPreEvaluationVal] {
      def postprocess(source: TmsForPreEvaluationVal)(implicit context: TmsExtractionContext): TmsForPreEvaluationVal =
        source.copy(tmsExtracted = source.tmsExtracted.postprocess)
    }
  }

  case class TmsForCollector(forType: Type,
                             // reverse order: head of forPreEvals is the last val in resulting code (most close to its for-yield)
                             forPreEvals: List[TmsForPreEvaluationVal],
                             // reverse order: head of forEnums is the last (near yield) `for` enumeration
                             forEnums: List[TmsForEnum]) {

    lazy val defSymbols: Seq[Symbol] = forPreEvals.flatMap(_.tmsExtracted.defSymbols) ++ forEnums.flatMap(_.tmsExtracted.defSymbols)

    def findRefSymbolOf(defSymbols: Set[Symbol]): Option[(Symbol, Position)] =
      forPreEvals.foldLeft(Option.empty[(Symbol, Position)])(_ orElse _.tmsExtracted.findRefSymbolOf(defSymbols)) orElse
        forEnums.foldLeft(Option.empty[(Symbol, Position)])(_ orElse _.tmsExtracted.findRefSymbolOf(defSymbols))

    def dependsOnIdent(ident: Ident): Boolean =
      forPreEvals.exists(_.tmsExtracted.dependsOnIdent(ident)) || forEnums.exists(_.tmsExtracted.dependsOnIdent(ident))

    def liftFor(yieldBody: Tree): Tree =
      q"..${forPreEvals.reverse}; for(..${forEnums.reverse}) yield $yieldBody"

    def reset: TmsForCollector = this.copy(forPreEvals = Nil, forEnums = Nil)

    def nonEmpty: Boolean = forEnums.nonEmpty

    def withForEnum(forEnum: TmsForEnum): TmsForCollector =
      this.copy(forEnums = forEnum :: forEnums)

    def withPreEvaluationVal(forPreEvaluationVal: TmsForPreEvaluationVal): TmsForCollector =
      this.copy(forPreEvals = forPreEvaluationVal :: forPreEvals)

    def forTypeName: String = forType.typeSymbol.name.toString

    def forTypeFullName: String = forType.typeSymbol.fullName

    def requiresPreEvaluation(implicit tmsOptions: TmsOptions, context: TmsExtractionContext): Boolean =
      tmsOptions.typeRequiresPreEvaluation(forTypeName, forTypeFullName) ||
        tmsOptions.mfTypePreEvaluation && typeConstructorConforms(forType, context.mfBaseType)

    def findReusableExtractedIdent(tmsExtracted: TmsExtracted)(implicit context: TmsExtractionContext): Option[Ident] =
      if (tmsExtracted.isValRefTree) {
        val extractedWithReusablePreEval = forPreEvals
          .find(_.tmsExtracted.equalsByRefTree(tmsExtracted))
          .map(pev => context.identWithMfSymbol(pev.termName))
          .fold(tmsExtracted) { reusablePreEvalIdent =>
            traceExtraction("findReusableExtractedIdent: found Pre Evaluation")(s"'val $reusablePreEvalIdent = $tmsExtracted'")
            TmsExtractedTree(reusablePreEvalIdent)
          }

        forEnums
          .find(_.tmsExtracted.equalsByRefTree(extractedWithReusablePreEval))
          .map {
            case arrowEnum: TmsForArrowEnum =>
              val arrowEnumIdent = Ident(arrowEnum.termName)
              traceExtraction("findReusableExtractedIdent: found For Arrow Enum")(s"'$arrowEnumIdent <- $extractedWithReusablePreEval'")
              arrowEnumIdent

            case assignEnum: TmsForAssignEnum =>
              val assignEnumIdent = context.identWithMfSymbol(assignEnum.termName)
              traceExtraction("findReusableExtractedIdent: found For Assign Enum")(s"'$assignEnumIdent = $extractedWithReusablePreEval'")
              findReusableExtractedIdent(TmsExtractedTree(assignEnumIdent)).getOrElse(
                c.abort(c.enclosingPosition, s"unexpected constrain error while findReusableExtractedIdent: not found TmsForArrowEnum with <- Ident(${assignEnum.termName}) which corresponds to $assignEnum")
              )
          }
      } else
        None

    // if tmsExtracted depends on any of Enum termName then tmsExtracted may not be added to PreEvaluationVal (due to forward ident reference otherwise)
    def extractedDependsOnEnumTermIdents(tmsExtracted: TmsExtracted)(implicit context: TmsExtractionContext): Boolean =
      forEnums.exists(fe => tmsExtracted.dependsOnIdent(context.identWithMfSymbol(fe.termName)))

    def withInlinedSinglePreEvalOfTopEnum(preEvalIdent: Ident, preEvalExtracted: TmsExtracted): TmsForCollector =
      // preEval term name is used in collector's Arrow Enum (never in the Arrow Enum)
      forEnums.lastOption match { // .last due to enums reverse order: it will be the top one in the `for`
        case None =>
          c.abort(c.enclosingPosition, s"unexpected constrain error while withInlinedSinglePreEvalOfTopEnum: collector has one pre evaluation val but has no enums: $this")

        case Some(arrowEnum@TmsForArrowEnum(_, extracted)) if extracted.equalsByRefTree(TmsExtractedTree(preEvalIdent)) =>
          this.copy(
            forPreEvals = Nil,
            forEnums = forEnums.init :+ arrowEnum.copy(tmsExtracted = preEvalExtracted)
          )

        case _ =>
          this
      }
  }

  object TmsForCollector {

    implicit val tmsForCollectorCodeViewer: TmsCodeViewer[TmsForCollector] = new TmsCodeViewer[TmsForCollector] {
      def codeView(source: TmsForCollector): String = {
        val valsCodeView = if (source.forPreEvals.nonEmpty)
          source.forPreEvals.reverse.map(_.codeView).mkString("", "\n", "\n")
        else ""

        valsCodeView +
        "for {\n" +
          source.forEnums.reverse.map(forArrowEnum => withIdent(forArrowEnum.codeView)).mkString("\n") +
        "\n} yield "
      }
    }

    implicit val tmsForCollectorPostprocessor: TmsExtractedPostprocessor[TmsForCollector] = new TmsExtractedPostprocessor[TmsForCollector] {
      def postprocess(source: TmsForCollector)(implicit context: TmsExtractionContext): TmsForCollector = {
        val withInlinedSinglePreEval = inlineSingleNonMfPreEval(source)
        withInlinedSinglePreEval.copy(
          forPreEvals = withInlinedSinglePreEval.forPreEvals.map(_.postprocess),
          forEnums = withInlinedSinglePreEval.forEnums.map(_.postprocess)
        )
      }
    }

    private def inlineSingleNonMfPreEval(tmsForCollector: TmsForCollector)(implicit context: TmsExtractionContext): TmsForCollector =
      if (tmsForCollector.forPreEvals.length == 1 && !context.isMfVal(tmsForCollector.forPreEvals.head.termName)) {
        // non Monadic Flow preEval term name is used strongly once in this collector's Arrow Enum
        val preEval = tmsForCollector.forPreEvals.head
        tmsForCollector.withInlinedSinglePreEvalOfTopEnum(Ident(preEval.termName), preEval.tmsExtracted)
      } else
        tmsForCollector

  }

  /** Contains for collectors for each type of Types Stack, contains methods for "opening" monad (extracting inner values) to requested types sequence
   *
   * @param reservedTypesNumber holds the maximum of accumulated number of already used stack types (number of outer stack types, including the head of the forCollectors) of monads stack.
   *                            In other words: the maximum number of types used by sibling expressions that forms ttsN expression (accumulating numbers used by nested tts).
   *                            These types are restricted for usage by the ttsN extraction after inner ttsM, ttsX, ...  of ttsN expression are extracted: `ttsN(expression = ttsM(ttsX(...) + ...) + )`
   *                            reservedTypesNumber is reset by withResetReservedTypes before extraction of tts expression and before extraction of inner Fors Stack to outer Fors Stack.
   *                            It is 0 for new TmsForsBuilder. The maximum of types used by siblings is set by withMaxOfReservedTypes while the chain extraction of sibling expressions.
   *                            The new value of used types is set to builder after tts extraction or extraction of inner Fors Stack -
   *                            places where stack types are "used" to reach the inner value of the monad - in extractMonadicValue.
   * @param forCollectors       head of forCollectors is the most outer type of the stack, most outer built `for` of the Stack
   */
  case class TmsForsBuilder private (reservedTypesNumber: Int,
                                    // direct order: head collector corresponds to most outer Stack Type
                                     forCollectors: List[TmsForCollector]) {

    lazy val defSymbols: Seq[Symbol] = forCollectors.flatMap(_.defSymbols)
    lazy val stackTypes: Seq[Type] = forCollectors.map(_.forType)

    def findRefSymbolOf(defSymbols: Set[Symbol]): Option[(Symbol, Position)] =
      forCollectors.foldLeft(Option.empty[(Symbol, Position)])(_ orElse _.findRefSymbolOf(defSymbols))

    def liftForsStack(yieldTree: Tree): Tree =
      forCollectors.filter(_.nonEmpty).foldRight(yieldTree) {
        case (forCollector, yieldBody) => forCollector.liftFor(yieldBody)
      }

    def reset: TmsForsBuilder = new TmsForsBuilder(
      reservedTypesNumber = 0,
      forCollectors = forCollectors.map(_.reset)
    )

    def withResetReservedTypes: TmsForsBuilder =
      if (reservedTypesNumber == 0) this else this.copy(reservedTypesNumber = 0)

    def withUpdatedReservedTypes(newReservedTypesNumber: Int): TmsForsBuilder =
      if (reservedTypesNumber == newReservedTypesNumber) this else this.copy(reservedTypesNumber = newReservedTypesNumber)

    def withMaxOfReservedTypes(other: TmsForsBuilder): TmsForsBuilder =
      if (reservedTypesNumber == other.reservedTypesNumber) this else this.copy(reservedTypesNumber = reservedTypesNumber.max(other.reservedTypesNumber))

    def withUpdatedForCollector(collectorIndex: Int, update: TmsForCollector => TmsForCollector): TmsForsBuilder =
      this.copy(forCollectors = forCollectors.updated(collectorIndex, update(forCollectors(collectorIndex))))

    def withForEnum(collectorIndex: Int, tmsForEnum: TmsForEnum): TmsForsBuilder =
      withUpdatedForCollector(collectorIndex, _.withForEnum(tmsForEnum))

    def withPreEvaluationVal(collectorIndex: Int, tmsForPreEvaluationVal: TmsForPreEvaluationVal): TmsForsBuilder =
      withUpdatedForCollector(collectorIndex, _.withPreEvaluationVal(tmsForPreEvaluationVal))

    def usedTypes: List[Type] = forCollectors.collect {
      case TmsForCollector(forType, _, forArrowEnums) if forArrowEnums.nonEmpty => forType
    }

    def getDegenerateForCollectorIndex(yieldTree: Tree): Int = {
      val lastNonEmptyIndex = forCollectors.lastIndexWhere(_.nonEmpty) // most inner non-empty for-collector index
      if (lastNonEmptyIndex != -1 && {
        val forArrowEnums = forCollectors(lastNonEmptyIndex).forEnums
        forArrowEnums.length == 1 && forArrowEnums.head.termName.toString == yieldTree.toString
      })
        lastNonEmptyIndex
      else
        -1
    }

    def flatMappableCollectorIndex(yieldType: Type): Int = {
      val yieldingForCollectorIndex = forCollectors.lastIndexWhere(_.nonEmpty) // most inner non-empty for-collector index
      if (yieldingForCollectorIndex == -1) {
        -1 // skips if nonEmpty forCollector does not exist
      } else if (!typeConstructorConforms(yieldType, forCollectors(yieldingForCollectorIndex).forType)) {
        -1 // skips flatMapping for yield type other than most inner non-empty forCollector type
      } else if (yieldingForCollectorIndex + 1 < forCollectors.length && typeConstructorConforms(yieldType, forCollectors(yieldingForCollectorIndex + 1).forType)) {
        -1 // skips flatMapping if the next inner type of Fors Stack (with empty collector) conforms to the yield type to be flatMapped. So, flatMapping action is controlled by Stack types: do not flat map when type is present in the Stack
      } else
        yieldingForCollectorIndex
    }

    /**
     * Adds Enums and optional Pre Evaluation val to TmsForCollector of each type passed
     * generating the chain of "opened" inner values of passed monadic `value`.
     * Validates possibility to add each type.
     * Finds & reuses already present chain (with possible extension) of "opening" stable val Idents.
     * Types should exist in non-reserved section of forCollectors in the specified order,
     * so passed types list may be non-continuous for non-reserved types in forCollectors
     * but should have the same order like non-reserved types have
     *
     * @param tmsExtracted monadic value to be "opened" to `types` level in this tmsForsBuilder Fors Stack
     * @param types reverse types of passed `value` to be "opened" (head = most inner type of monads stack: M2 in `tmsFor(M1[M2[Int]]`) & M2 in tts2[M1, M2, Int])
     * @return resulting TmsForsBuilder with updated forCollectors and reservedTypesNumber + Ident that represents inner value of passed monad `value` at last of `types` level
     */
    def extractMonadicValue(types: Seq[Type], tmsExtracted: TmsExtracted, tmsExpression: TmsExpression)(implicit context: TmsExtractionContext): TmsForExtract[Tree] =
      extractMonadicValue(types, tmsExtracted, tmsExpression, firstType = true)

    @tailrec
    private def extractMonadicValue(types: Seq[Type], tmsExtracted: TmsExtracted, tmsExpression: TmsExpression, firstType: Boolean)(implicit context: TmsExtractionContext): TmsForExtract[Tree] = {
      val headType = types.head
      val collectorIndex = validatedCollectorIndexForType(headType, tmsExpression)

      if (firstType) validateExtractedIsIndependentOfCollectorsAndYieldDefs(collectorIndex, tmsExtracted)

      val incrementedReservedTypesBuilder = if (reservedTypesNumber - 1 < collectorIndex) // if non-reserved type of the stack is used
        this.withUpdatedReservedTypes(collectorIndex + 1)
      else
        this

      // Monadic Flow vals are always added to Monadic Flow Type forCollector to PreEvaluationVals (if option is on) or
      // as Assign For Enum in the TmsBlock extractMonadicFlow (to be accessed directly by its name without tts).
      // First Monadic Flow val (when it is the first MFC block statement) is always added to PreEvaluationVals (even if PreEval option is off).
      // So Monadic Flow val are always found by reusage mechanism and it does not require adding to PreEvaluationVals here
      val headTypeExtract = incrementedReservedTypesBuilder.forCollectors(collectorIndex)
        .findReusableExtractedIdent(tmsExtracted)
        .map { reusableIdent =>
          traceExtraction("extractMonadicValue: Reused Ident")(s"$reusableIdent for $tmsExtracted")

          TmsForExtract[Tree](incrementedReservedTypesBuilder, reusableIdent)
        }
        .getOrElse {
          val (builderWithPreEval, extractedWithPreEval) = if (firstType)
            incrementedReservedTypesBuilder.withOptionallyAddedPreEvaluation(collectorIndex, tmsExtracted)
          else
            (incrementedReservedTypesBuilder, tmsExtracted)

          val enumTermName = c.freshName(TermName(s"valueOf${headType.typeSymbol.name.toString}$$"))
          val tmsForArrowEnum = TmsForArrowEnum(enumTermName, extractedWithPreEval)
          val builderWithEnum = builderWithPreEval.withForEnum(collectorIndex, tmsForArrowEnum)

          traceExtraction("extractMonadicValue: Added For Arrow Enum")(s"'$enumTermName <- $extractedWithPreEval'")

          TmsForExtract[Tree](builderWithEnum, Ident(enumTermName))
        }

      if (types.length == 1)
        headTypeExtract
      else
        headTypeExtract.forsBuilder.extractMonadicValue(types.tail, TmsExtractedTree(headTypeExtract.extract), tmsExpression, firstType = false)
    }

    private def withOptionallyAddedPreEvaluation(collectorIndex: Int, tmsExtracted: TmsExtracted)(implicit tmsOptions: TmsOptions, context: TmsExtractionContext): (TmsForsBuilder, TmsExtracted) = {
      val forCollector = forCollectors(collectorIndex)
      if (forCollector.requiresPreEvaluation && // if type is defined in the tmsOptions as to be pre evaluated
        !forCollector.extractedDependsOnEnumTermIdents(tmsExtracted) &&  // if extracted tree does not depend on any Enum termNames including mfc type vals (with own symbols) already present in the forCollector
        (context.mfForCollectorIndex == collectorIndex || !tmsExtracted.isNonLazyValRefTree) // skips pre eval of Non Lazy stable Val only for NON Monadic Flow Collector (MFC Collector pre evals may be chained: one may depend on previous mfc val - so to don't break the chain of preEvaluations)
      ) {
        val preEvalTermName = c.freshName(TermName(s"val${forCollector.forTypeName}$$"))
        val builderWithPreEval = withPreEvaluationVal(collectorIndex, TmsForPreEvaluationVal(preEvalTermName, tmsExtracted))

        traceExtraction("extractMonadicValue: Added Pre Evaluation")(s"'val $preEvalTermName = $tmsExtracted'")

        (builderWithPreEval, TmsExtractedTree(Ident(preEvalTermName)))
      } else
        (this, tmsExtracted)
    }

    private def validatedCollectorIndexForType(tpe: Type, tmsExpression: TmsExpression): Int = {
      val typeIndex = forCollectors.indexWhere(fc => typeConstructorConforms(tpe, fc.forType), (reservedTypesNumber - 1).max(0) /*max is fix for 2.11 when -1*/) // even if tpe conforms to the last reserved Type: we may use it
      if (typeIndex == -1) {
        val reservedTypes = formattedTypeNames(stackTypes.take(reservedTypesNumber))
        val nonReservedTypes = formattedTypeNames(stackTypes.drop(reservedTypesNumber))
        c.abort(tmsExpression.pos, s"type $tpe could not be found in the non-reserved rest of the types stack $nonReservedTypes. " +
          s"Reserved types (from outer to the last used inner one inclusive) are $reservedTypes. " +
          "Macros result type (value of the type parameter) and tms type of the the macros input expression are different")
      } else
        typeIndex
    }

    def validateExtractedIsIndependentOfCollectorsAndYieldDefs(toCollectorIndex: Int, tmsExtracted: TmsExtracted)(implicit context: TmsExtractionContext): Unit = {
      def without211lazyVals(symbols: Set[Symbol]): Set[Symbol] = if (util.Properties.versionNumberString >= "2.12") symbols else symbols.filterNot(isLazyTerm) // skips error of impossible lazy val usage for 2.11
      // 2.11 emits 2 definitions for lazy val in typechecked code but only 1 definition for the same untypechecked (reconstructed) code: it does not emits accessor def with original name of the val.
      // This makes not possible to correctly detect lazy val improper usage in some cases. Instead, 2.11 compiler will emit corresponding error in such cases,
      // but outer scope value with the same name may accidentally be used instead: generated code is not hygienic for these lazy vals in 2.11

      val tmsExtractedDefSymbols = traceSymbolsSet(s"Definition Symbols of $tmsExtracted")(tmsExtracted.defSymbols)
      // excluding symbols defined in tmsExtracted - local defs
      val nonReachableDefSymbols = without211lazyVals(if (context.mfForCollectorIndex <= toCollectorIndex) {
        context.nonMfValDefSymbols -- tmsExtractedDefSymbols // when we add tmsExtracted to mfForCollector or inner one: mfVals are visible
      } else {
        context.inputDefSymbols -- tmsExtractedDefSymbols // when we add tmsExtracted to collector outer than mfForCollector: mfVals are not visible
      })

      tmsExtracted.findRefSymbolOf(nonReachableDefSymbols).foreach { case (nonReachableSymbol, refPosition) =>
        c.abort(refPosition, s"locally defined (in tmsCode) '$nonReachableSymbol' may not be used inside such expression " +
          s"since this expression should be used in Fors Stack outside of '$nonReachableSymbol' definition scope. " +
          "If such definition initializer/body does not depend on tts then try to move this definition outside of macro code. " +
          "If 'Fors Stack For Apply Source' or 'Fors Stack For Apply Parameter` option is enabled then try to disable ones")
      }
    }
  }

  object TmsForsBuilder {
    def apply(forTypes: List[Type]): TmsForsBuilder =
      traceExtraction("Created new TmsForsBuilder") {
        TmsForsBuilder(0, forTypes.map(TmsForCollector(_, Nil, Nil)))
      }

    implicit val tmsForsBuilderPostprocessor: TmsExtractedPostprocessor[TmsForsBuilder] = new TmsExtractedPostprocessor[TmsForsBuilder] {
      def postprocess(source: TmsForsBuilder)(implicit context: TmsExtractionContext): TmsForsBuilder =
        source.copy(forCollectors = source.forCollectors.map(_.postprocess))
    }
  }
}

