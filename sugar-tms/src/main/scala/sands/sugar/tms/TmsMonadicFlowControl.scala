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
 * at 10.05.2021 14:22
 */

private[tms] trait TmsMonadicFlowControl { _: TmsTree =>
  import c.universe._

  object MonadicFlowControl {
    val UnitType: Type = typeOf[Unit]

    def buildExtractionContext(tmsExpression: TmsExpression, forsStackTypes: List[Type], inputDefSymbols: Set[Symbol]): TmsExtractionContext = {
      val mfStatements = inputMfStatements(tmsExpression)
      val firstStatementTypeIndex: Int = forsStackTypeIndex(mfStatements, forsStackTypes)
      val mfBaseType: Type = traceMonadicFlow("Type")(forsStackTypes(firstStatementTypeIndex).typeConstructor)
      val (mfForEnumStats, _) = splitToEnumAndYieldStatements(mfStatements, mfBaseType)
      val enumValDefSymbols = traceSymbolsSet("Monadic Flow Enum val def Symbols")(mfForEnumStats.collect {
        case enumVal: TmsValVarDef  => enumVal.symbol // of mf and non-mf types, so `<-` & `=` vals
      }.toSet)

      TmsExtractionContext(mfBaseType, firstStatementTypeIndex, enumValDefSymbols, inputDefSymbols)
    }

    private def forsStackTypeIndex(mfStatements: Seq[TmsStatement], forsStackTypes: List[Type]) = {
      val firstStatement = mfStatements.head
      val firstStatementType = typeOfMonadicFlowEnumStatement(firstStatement)
      val firstStatementTypeIndex = forsStackTypes.indexWhere(isMonadicFlowType(firstStatementType, _))
      if (firstStatementTypeIndex < 0) {
        val stackTypes = formattedTypeNames(forsStackTypes)
        abortMfc(firstStatement.pos, s"Monadic Flow Type $firstStatementType detected by the first statement does not conform to any of the Fors Stack types $stackTypes.")
      }
      firstStatementTypeIndex
    }

    private def abortMfc(pos: Position, prefix: String): Nothing =
      c.abort(pos, s"$prefix $MfcInvalidInputError")

    private def inputMfStatements(tmsExpression: TmsExpression): Seq[TmsStatement] =
      tmsExpression match {
        case tmsBlock: TmsBlock => tmsBlock.statements
        case singleExpression => Seq(singleExpression)
      }

    private def typeOfMonadicFlowEnumStatement(tmsStatement: TmsStatement): Type = tmsStatement match {
      case TmsValVarDef(_, true, symbol, _, _, tpt, _) if !isLazyTerm(symbol) => tpt.tpe
      case otherTree: TmsOtherTree if otherTree.tree.isDef => abortMfc(otherTree.pos, MfcInvalidStatementMessage) // to abort when type def
      case tmsExpression: TmsExpression => tmsExpression.tpe
      case otherStatement => abortMfc(otherStatement.pos, MfcInvalidStatementMessage)
    }

    private def isMonadicFlowType(tpe: Type, mfBaseType: Type): Boolean = typeConstructorConforms(tpe, mfBaseType)

    private def isMfTypeExpression(tmsStatement: TmsStatement, mfBaseType: Type): Boolean = tmsStatement match {
      case expression: TmsExpression => isMonadicFlowType(expression.tpe, mfBaseType)
      case _ => false
    }

    private def isMfTypeExpressionOrStableValDef(tmsStatement: TmsStatement, mfBaseType: Type): Boolean = tmsStatement match {
      case tmsVal: TmsValVarDef if tmsVal.isVal => !isLazyTerm(tmsVal.symbol)
      case tmsExpression: TmsExpression => isMonadicFlowType(tmsExpression.tpe, mfBaseType)
      case _ => false
    }

    /** Splits mfStatements to validated Enum Statements and the rest Yield Statements:
     *
     * enumStatements - start of mfStatements including the last stable non-lazy val and last expression of mfBaseType type - For Enums.
     *
     * yieldStatements - end of statements after the last stable non-lazy val or last expression of mfBaseType type - For Yield.
     *
     * If yieldStatements become empty then last mfBaseType Enum Expression (not a val: yield will be a Unit for a val)
     * is moved to yieldStatements (to correspond to Fors Stack return type) to be flatMapped by postprocessing
     */
    private def splitToEnumAndYieldStatements(mfStatements: Seq[TmsStatement], mfBaseType: Type): (Seq[TmsStatement], Seq[TmsStatement]) = {
      val lastIndexOfEnumStatement = mfStatements.lastIndexWhere(isMfTypeExpressionOrStableValDef(_, mfBaseType))
      val enumStatements = mfStatements.take(lastIndexOfEnumStatement + 1)
      val yieldStatements = mfStatements.drop(lastIndexOfEnumStatement + 1)
      // it validates for-enum statements for Monadic Flow compliance as side effect
      enumStatements.foreach(typeOfMonadicFlowEnumStatement)

      if (yieldStatements.isEmpty && isMfTypeExpression(enumStatements.last, mfBaseType))
        // when yield is empty: if the last block statement is expression (not a val) of mfBaseType then move it to yield to correspond to initial tmsCode Type -
        // it will be flatMapped (moved to the last for enum) by postprocessing (otherwise postprocessing will fail)
        (enumStatements.init, Seq(enumStatements.last))
      else
        (enumStatements, yieldStatements)
    }

    /** Current implementation correctly works only for root Block (tmsCode): it does not return min reservedTypesNumber of used inner tts of the input Block
     */
    def extractRootBlock(tmsExpression: TmsExpression, tmsForsBuilder: TmsForsBuilder)(implicit context: TmsExtractionContext): TmsForExtract[Tree] = {
      val mfStatements = inputMfStatements(tmsExpression)
      val mfBaseType: Type = context.mfBaseType
      val (mfForEnumStats, mfForYieldStats) = splitToEnumAndYieldStatements(mfStatements, mfBaseType)

      def isValOrMfTypeExpression(tmsStatement: TmsStatement): Boolean = tmsStatement match {
        case _: TmsValVarDef => true // lazy vals are absent here after validation above
        case _ => isMfTypeExpression(tmsStatement, mfBaseType)
      }

      // wraps non-val & non-mfType 2+ expressions subsequences to Blocks
      def groupNonMfTypeExpressions(statements: Seq[TmsStatement]): Seq[TmsStatement] =
        if (statements.isEmpty)
          Nil
        else {
          val valOrMfTypeStats = statements.takeWhile(isValOrMfTypeExpression) // span in Scala 3
          val restMixed = statements.dropWhile(isValOrMfTypeExpression)
          val nonMfTypeExpressions = restMixed.takeWhile(st => !isValOrMfTypeExpression(st)) // span in Scala 3
          val tailStats = restMixed.dropWhile(st => !isValOrMfTypeExpression(st))
          val groupedNonMfTypeExpressions = nonMfTypeExpressions.length match {
            case 0 => Nil
            case 1 => nonMfTypeExpressions
            case _ => Seq(TmsBlock(nonMfTypeExpressions.head.pos, UnitType, nonMfTypeExpressions))
          }
          valOrMfTypeStats ++ groupedNonMfTypeExpressions ++ groupNonMfTypeExpressions(tailStats)
        }

      val enumsForsBuilder: TmsForsBuilder = groupNonMfTypeExpressions(mfForEnumStats).foldLeft(tmsForsBuilder) { (builder, enumStatement) =>
        enumStatement match {
          case tmsVal: TmsValVarDef => extractValDef(tmsVal, builder)
          case tmsExpression: TmsExpression => extractExpression(tmsExpression, builder)
          case otherStatement => c.abort(otherStatement.pos, s"unexpected constrain error while Monadic Flow Control Block extraction: $MfcInvalidStatementMessage")
        }
      }

      def wrapToBlock(statements: Seq[TmsStatement]): TmsBlock = {
        val blockType = statements.lastOption match {
          case Some(tmsOtherTree: TmsOtherTree) if tmsOtherTree.tree.isDef => UnitType // type def
          case Some(tmsExpression: TmsExpression) => tmsExpression.tpe
          case _ => UnitType
        }
        TmsBlock(if (statements.nonEmpty) statements.head.pos else mfForEnumStats.last.pos,  blockType, statements)
      }

      val mfForYieldStatement = mfForYieldStats match {
        case Seq(tmsOtherTree: TmsOtherTree) if tmsOtherTree.tree.isDef => wrapToBlock(mfForYieldStats) // single type def
        case Seq(expression: TmsExpression) => expression // single expression
        case _ => // including single definition statement & empty sequence
          wrapToBlock(mfForYieldStats)
      }

      traceMonadicFlow("Block extract") {
        mfForYieldStatement.extract(enumsForsBuilder)
      }
    }

    /** tmsVal should be already verified to be a stable non-lazy val */
    private def extractValDef(tmsVal: TmsValVarDef, tmsForsBuilder: TmsForsBuilder)(implicit context: TmsExtractionContext): TmsForsBuilder = {
      val TmsForExtract(initializerForsBuilder, initializerExtract) = tmsVal.initializer.extract(tmsForsBuilder)
      val mfCollectorIndex = nonConsumedMonadicFlowCollectorIndex(initializerForsBuilder, tmsVal.initializer)
      val initializerExtractedTree = TmsExtractedTree(initializerExtract)

      initializerForsBuilder.validateExtractedIsIndependentOfCollectorsAndYieldDefs(mfCollectorIndex, initializerExtractedTree)

      def initializerForsBuilderWithAssignEnum(): TmsForsBuilder = initializerForsBuilder.withForEnum(mfCollectorIndex, TmsForAssignEnum(tmsVal.name, initializerExtractedTree))

      if (isMonadicFlowType(tmsVal.tpt.tpe, context.mfBaseType)) {
        val mfCollector = initializerForsBuilder.forCollectors(mfCollectorIndex)
        val shouldAddPreEvaluationVal = mfCollector.forEnums.isEmpty || // first For Enum may not be Assign Enum, so always only as outside val = pre evaluation
          mfCollector.requiresPreEvaluation && // if type is defined in the tmsOptions as to be pre evaluated
            !mfCollector.extractedDependsOnEnumTermIdents(initializerExtractedTree) // if extracted tree does not depend on any Enum termNames including mf type vals (with own symbols) already present in the forCollector
            //! do not skip pre evaluation of stable non-lazy val initializers: MFC pre evaluations may be chained - may depend on previous one directly (as non-opened monadic value)

        val tmsValAssignForsBuilder = if (shouldAddPreEvaluationVal)
          initializerForsBuilder.withPreEvaluationVal(mfCollectorIndex, TmsForPreEvaluationVal(tmsVal.name, initializerExtractedTree))
        else
          initializerForsBuilderWithAssignEnum()

        val valueTermName = c.freshName(TermName(s"${tmsVal.name.toString}Value$$"))
        // original Symbol of the tmsVal is set to its Ident to be found by reusage mechanism: when tts1(tmsVal.name) will be extracted then already "open" `valueTermName <- tmsVal.name` will be found by this Symbol matching
        val tmsValOriginalIdent = c.internal.setSymbol(Ident(tmsVal.name), tmsVal.symbol)
        tmsValAssignForsBuilder.withForEnum(mfCollectorIndex, TmsForArrowEnum(valueTermName, TmsExtractedTree(tmsValOriginalIdent)))
      } else
        initializerForsBuilderWithAssignEnum()
    }

    private def extractExpression(tmsExpression: TmsExpression, tmsForsBuilder: TmsForsBuilder)(implicit context: TmsExtractionContext): TmsForsBuilder = {
      val TmsForExtract(expressionForsBuilder, expressionExtract) = tmsExpression.extract(tmsForsBuilder)
      val mfCollectorIndex = nonConsumedMonadicFlowCollectorIndex(expressionForsBuilder, tmsExpression)
      val expressionExtractedTree = TmsExtractedTree(expressionExtract)

      expressionForsBuilder.validateExtractedIsIndependentOfCollectorsAndYieldDefs(mfCollectorIndex, expressionExtractedTree)

      if (isMonadicFlowType(tmsExpression.tpe, context.mfBaseType)) {
        val mfCollector = expressionForsBuilder.forCollectors(mfCollectorIndex)
        val shouldAddPreEvaluationVal = mfCollector.requiresPreEvaluation && // if type is defined in the tmsOptions as to be pre evaluated
          !mfCollector.extractedDependsOnEnumTermIdents(expressionExtractedTree) && // if extracted tree does not depend on any Enum termNames including mf type vals (with own symbols) already present in the forCollector
          !expressionExtractedTree.isNonLazyValRefTree // skips pre evaluation of stable non-lazy val: it has no sense for wildcard `_ <- stableNonLazyVal` for-enum

        val wcMfFreshTermName = c.freshName(TermName("wcMf$"))
        if (shouldAddPreEvaluationVal) {
          val preEvalTermName = c.freshName(TermName(s"val${mfCollector.forTypeName}$$"))
          val withPreEvalForsBuilder = expressionForsBuilder.withPreEvaluationVal(mfCollectorIndex, TmsForPreEvaluationVal(preEvalTermName, expressionExtractedTree))

          val forArrowEnum = TmsForArrowEnum(wcMfFreshTermName, TmsExtractedTree(Ident(preEvalTermName)))
          withPreEvalForsBuilder.withForEnum(mfCollectorIndex, forArrowEnum)
        } else {
          val forArrowEnum = TmsForArrowEnum(wcMfFreshTermName, expressionExtractedTree)
          expressionForsBuilder.withForEnum(mfCollectorIndex, forArrowEnum)
        }
      } else {
        val forAssignEnum = TmsForAssignEnum(c.freshName(TermName("wcNonMf$")), expressionExtractedTree)
        expressionForsBuilder.withForEnum(mfCollectorIndex, forAssignEnum)
      }
    }

    def nonConsumedMonadicFlowCollectorIndex(forsBuilder: TmsForsBuilder, lastExtractedExpression: TmsExpression)(implicit context: TmsExtractionContext): Int =
      if (context.mfForCollectorIndex < forsBuilder.reservedTypesNumber - 1) {
        val stackTypes = formattedTypeNames(forsBuilder.stackTypes)
        c.abort(lastExtractedExpression.pos,
          s"extraction of the expression has consumed the part of Types Stack $stackTypes including type(s) inner than Monadic Flow Type ${context.mfBaseType}. " +
          "Types of the Stack which are inner than Monadic Flow Type may be used only in tts of the yield expression (not in the for enum expressions). " +
          "So extraction of this expression (at the level of Monadic Flow Type) becomes not possible since otherwise it depends on expression that is inner " +
          "in Types Stack than Monadic Flow Type")
      } else // even if the last reserved type is Monadic Flow Type: we may add to it
        context.mfForCollectorIndex

    def postprocessExtractedForsStack(forsStack: TmsExtractedForsStack)(implicit context: TmsExtractionContext): TmsExtractedForsStack =
      if (tmsOptions.monadicFlowControl) {
        val withPostProcessedPreEval = inlineSingleNonUsedMfPreEval(forsStack)
        inlineNonUsedMfEnumVals(withPostProcessedPreEval)
      } else
        forsStack

    private def inlineSingleNonUsedMfPreEval(forsStack: TmsExtractedForsStack)(implicit context: TmsExtractionContext): TmsExtractedForsStack = {
      val mfCollectorIndex = context.mfForCollectorIndex
      val mfCollector = forsStack.forsBuilder.forCollectors(mfCollectorIndex)
      if (mfCollector.forPreEvals.length == 1 && context.isMfVal(mfCollector.forPreEvals.head.termName)) {
        // Monadic Flow mfPreEval term name may used in this collector's Arrow Enum & anywhere in inner forCollectors or in the yield body
        val mfPreEval = mfCollector.forPreEvals.head
        val mfIdent = context.identWithMfSymbol(mfPreEval.termName)

        if (!forsStack.dependsOnIdent(mfIdent, mfCollectorIndex + 1)) {
          val inlinedPreEvalMfForCollector = mfCollector.withInlinedSinglePreEvalOfTopEnum(mfIdent, mfPreEval.tmsExtracted)
          if (inlinedPreEvalMfForCollector.dependsOnIdent(mfIdent))
            forsStack
          else
            forsStack.copy(
              forsBuilder = forsStack.forsBuilder.withUpdatedForCollector(mfCollectorIndex, _ => inlinedPreEvalMfForCollector)
            )
        } else
          forsStack
      } else
        forsStack
    }

    /** Inlines all pairs 'mfVal = mfInit' and 'mfValue <- mfVal' to single 'mfValue <- mfInit'
     * starting fromIndex of Monadic Flow collector Enums where mfVal is used only in this pair
     * (is not used in the rest of this or inner for-collectors or yield body of passed fors stack) */
    @tailrec
    private def inlineNonUsedMfEnumVals(forsStack: TmsExtractedForsStack, fromIndex: Int = 0)(implicit context: TmsExtractionContext): TmsExtractedForsStack = {
      val mfCollectorIndex = context.mfForCollectorIndex
      val mfCollector = forsStack.forsBuilder.forCollectors(mfCollectorIndex)
      // keep in mind the reverse order of mfEnums: head is the most inner for-enum,
      // so in the pair 'mfValIdent = mfInit' and 'mfValue <- mfValIdent' the arrow Enum 'mfValue <- ...' has less index
      val mfEnums = mfCollector.forEnums
      val arrowIndex = mfEnums.indexWhere({
        case TmsForArrowEnum(_, extracted) => extracted.isValRefTree
        case _ => false
      }, fromIndex)

      if (arrowIndex == -1 || arrowIndex == mfEnums.length - 1) // when not found or found but the last enum
        forsStack // end of recursion
      else
        mfEnums(arrowIndex + 1) match {
          case TmsForAssignEnum(valTermName, valInit) =>
            val arrowEnum = mfEnums(arrowIndex)
            val valIdent = context.identWithMfSymbol(valTermName)
            if (arrowEnum.tmsExtracted.equalsByRefTree(TmsExtractedTree(valIdent)) && // when assign Enum corresponds to arrow Enum by valIdent name
              !forsStack.dependsOnIdent(valIdent, mfCollectorIndex + 1) // when fors Stack yield & inner forCollectors do not depend on valIdent
            ) {
              val inlinedMfCollector = mfCollector.copy(
                forEnums = (mfEnums.take(arrowIndex) :+ TmsForArrowEnum(arrowEnum.termName, valInit)) ++ mfEnums.drop(arrowIndex + 2)
              )
              if (inlinedMfCollector.dependsOnIdent(valIdent))
                inlineNonUsedMfEnumVals(forsStack, arrowIndex + 2) // search the rest of Enums skipping both found arrow & assign enums
              else {
                val inlinedForsStack = forsStack.copy(
                  forsBuilder = forsStack.forsBuilder.withUpdatedForCollector(mfCollectorIndex, _ => inlinedMfCollector)
                )
                inlineNonUsedMfEnumVals(inlinedForsStack, arrowIndex + 1) // search the rest of Enums skipping enums pair just inlined to 1 arrow enum
              }
            } else
              inlineNonUsedMfEnumVals(forsStack, arrowIndex + 2) // search the rest of Enums skipping both found arrow & assign enums

          case _ =>
            inlineNonUsedMfEnumVals(forsStack, arrowIndex + 1) // search the rest of Enums skipping found arrow enum
        }
    }


    private val MfcInvalidStatementMessage = "invalid Monadic Flow Control statement."
    private val MfcInvalidInputError: String =
      "Monadic Flow Control input code should be a block with the first stable non-lazy val definition or with the first expression " +
        "of the type which conforms to one of the macros Monadic Stack Types (first outer suitable one, referenced as Monadic Flow type). " +
        "Only stable non-lazy val definitions and any type expressions are enabled in this root block before the last statement of the Monadic Flow type " +
        "(ones go to for-enum part) since other terms or types definitions do not have `for` enum equivalents and could not be transformed to for-enum " +
        "keeping one's original visibility scope. Such other definitions may be used after the last statement of Monadic Flow type " +
        "(ones go to for-yield part) or inside inner blocks of the Monadic Flow input code"
  }

}
