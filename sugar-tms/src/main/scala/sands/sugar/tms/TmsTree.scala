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

//import scala.Predef.??? //uncomment and see very strange bugs (at least in 2.13) ???)))
import scala.reflect.macros.blackbox

/*
 * Created by Serhiy Shamshetdinov
 * at 08.12.2020 13:47
 */

/** Tms expression BNF like tree syntax. If scala tree does not contain ttsNxxx implicit or one is unreachable by this AST then it is unlifted as TmsOtherTree.
 * {{{
 *
 * TmsExpression    ::= TmsApply |
 *                      TmsFunction |
 *                      TmsCombinedExpr | // if, partial function, match, try, do, while, var assignment, return
 *                      TmsBlock | // of TmsStatement
 *                      TmsNew |
 *                      TmsAscripted | // ascription and/or annotation
 *                      TmsOtherTree // stays Scala tree: everything other that is not covered by tms syntax tree (type, literals, etc.) or parsed but may be not used or not depend on ttsNxxx
 *
 * TmsApply         ::= TmsExpression.termName[..types](...arguments: TmsExpression) |
 *                      termName[..types](...arguments: TmsExpression) |
 *                      typeTree(...arguments: TmsExpression) // applied type (parent in new class[T](...args))
 *
 * TmsAscripted     ::= TmsExpression: [Tpe] {@annotations} // includes the variant with the only annotation(s)
 * 
 * TmsStatement     ::= TmsValVarDef |
 *                      TmsDefDef |
 *                      TmsTraitDefQQ |
 *                      TmsClassDef |
 *                      TmsModuleDef |
 *                      TmsExpression
 *
 * }}}
 */
private[tms] trait TmsTree extends TmsMonadicFlowControl with TmsTreePreprocessing with TmsForExtraction with TmsTools {
  val c: blackbox.Context
  import c.universe._

  implicit def tmsOptions: TmsOptions

  private val ttsImplicitsHolderPackage = TransparentMonads.getClass.getName.init // just in case TmsImplicits be in other package (now it is the same)


  // "Expressions are computable statements": https://docs.scala-lang.org/tour/basics.html :-)
  sealed trait TmsExpression extends TmsStatement {
    def tpe: Type
    def nonAscripted: TmsExpression = this
  }

  sealed trait TmsStatement {
    def pos: Position
    def dependsOnTts: Boolean
    def hasTts: Boolean = dependsOnTts // for all expressions except TmsApply (only TmsApply maybe tts and its hasTts may be != dependsOnTts, so for others hasTts = dependsOnTts)
  }

  case class TmsApply(pos: Position,
                      tpe: Type,
                      maybeTermSource: Option[TmsExpression],
                      termName: TermName,
                      maybeIdentSymbol: Option[Symbol], // only Symbol of the locally defined method call which has tts parameter in the call to be restored for postprocessing validation of non-scope usage
                      types: Seq[Tree],
                      arguments: Seq[Seq[TmsExpression]]) extends TmsExpression {
    lazy val isTts: Boolean = maybeTermSource.exists(TmsApply.applyIsTts(_, termName))
    lazy val dependsOnTts: Boolean = maybeTermSource.exists(_.hasTts) || existsHasTts(arguments)
    override lazy val hasTts: Boolean = isTts || dependsOnTts
    def firstArgument: TmsExpression = arguments.head.head
  }

  case class TmsFunction(pos: Position,
                         tpe: Type,
                         params: Seq[TmsValVarDef],
                         body: TmsExpression) extends TmsExpression {
    lazy val dependsOnTts: Boolean = body.hasTts
  }

  case class TmsCombinedExpr(pos: Position,
                             tpe: Type,
                             ceType: TmsCombinedExpr.CombinedExprType,
                             expressions: Seq[Seq[TmsExpression]]) extends TmsExpression {
    lazy val dependsOnTts: Boolean = existsHasTts(expressions)
  }

  case class TmsBlock(pos: Position,
                      tpe: Type,
                      statements: Seq[TmsStatement]) extends TmsExpression {
    lazy val dependsOnTts: Boolean = statements.exists(_.hasTts)
  }

  case class TmsNew(pos: Position,
                    tpe: Type,
                    appliedTypeExpression: TmsExpression) extends TmsExpression {
    lazy val dependsOnTts: Boolean = appliedTypeExpression.hasTts
  }

  // using QQ q"new ..." full pattern (TmsNewQQ) as primary unlifter of new expression (unlifts block of {class $anon extends Parent(initExpression) with ...; new $anon}) fails to unlift initExpression
  // when initExpression is implicitly converted class initializer value (when class extends Trait or has refinement body) (like TmsClassDefQQ & TmsObjectDefQQ fails)
  // Failed Tests samples (+ others TOOLBOX ones):
  // TmsForReachableSyntaxTest.scala:2011 "process new case class with trait with 2 tts in parameters: NON-TOOLBOX"
  // TmsForReachableSyntaxTest.scala:2039 "process new case class with refinement body with tts in parameter: NON-TOOLBOX"
//  case class TmsNewQQ(pos: Position,
//                      tpe: Type,
//                      earlyValDefs: Seq[TmsValVarDef],
//                      parents: Seq[TmsExpression],
//                      selfValDef: TmsValVarDef,
//                      statements: Seq[TmsStatement]) extends TmsExpression {
//    lazy val dependsOnTts: Boolean = existsHasTts(earlyValDefs) || existsHasTts(parents) || existsHasTts(statements)
//  }

  case class TmsAscription(pos: Position,
                           tpe: Type,
                           expression: TmsExpression,
                           ascription: Tree) extends TmsExpression {
    lazy val dependsOnTts: Boolean = expression.hasTts
    override def nonAscripted: TmsExpression = expression.nonAscripted
  }

  case class TmsOtherTree(tree: Tree, untypecheck: Boolean = true) extends TmsExpression {
    def pos: Position = tree.pos
    val dependsOnTts = false
    def tpe: Type = tree.tpe
//    override def toString: String = s"TmsOtherTree($tree) tree: ${showRaw(tree)}"
  }

  case class TmsValVarDef(pos: Position,
                          isVal: Boolean,
                          symbol: Symbol,
                          modifiers: Modifiers,
                          name: TermName,
                          tpt: Tree,
                          initializer: TmsExpression) extends TmsStatement {
    lazy val dependsOnTts: Boolean = initializer.hasTts
  }

  case class TmsDefDef(pos: Position,
                       symbol: Symbol,
                       modifiers: Modifiers,
                       name: TermName,
                       typeParameters: Seq[TypeDef],
                       parameters: Seq[Seq[TmsValVarDef]],
                       returnType: Tree,
                       body: TmsExpression) extends TmsStatement {
    lazy val dependsOnTts: Boolean = body.hasTts || existsHasTts(parameters)
  }

  case class TmsClassDef(pos: Position,
                         symbol: Symbol,
                         modifiers: Modifiers,
                         typeName: TypeName,
                         typeParameters: Seq[TypeDef],
                         template: TmsTemplate) extends TmsStatement {
    lazy val dependsOnTts: Boolean = template.dependsOnTts
  }

  // object
  case class TmsModuleDef(pos: Position,
                          symbol: Symbol,
                          modifiers: Modifiers,
                          termName: TermName,
                          template: TmsTemplate) extends TmsStatement {
    lazy val dependsOnTts: Boolean = template.dependsOnTts
  }

  case class TmsTemplate(parents: Seq[TmsExpression],
                         selfValDef: TmsValVarDef,
                         statements: Seq[TmsStatement]) {
    lazy val dependsOnTts: Boolean = existsHasTts(parents) || existsHasTts(statements)
  }

  case class TmsTraitDefQQ(pos: Position,
                           symbol: Symbol,
                           modifiers: Modifiers,
                           typeName: TypeName,
                           typeParameters: Seq[TypeDef],
                           earlyValDefs: Seq[TmsValVarDef],
                           parents: Seq[TmsExpression],
                           selfValDef: TmsValVarDef,
                           statements: Seq[TmsStatement]) extends TmsStatement {
    lazy val dependsOnTts: Boolean = existsHasTts(earlyValDefs) || existsHasTts(parents) || existsHasTts(statements)
  }

  // using QQ q"class ..." full pattern (TmsClassDefQQ) as unlifter of class definition "class $anon extends Parent(initExpression) with ..." fails to unlift initExpression
  // when initExpression is implicitly converted class initializer value (when class extends Trait or has refinement body) (like TmsNewQQ & TmsObjectDefQQ fails)
  // Failed Tests samples (+ others TOOLBOX ones):
  // TmsForReachableSyntaxTest.scala:2011 "process new case class with trait with 2 tts in parameters: NON-TOOLBOX"
  // TmsForReachableSyntaxTest.scala:2039 "process new case class with refinement body with tts in parameter: NON-TOOLBOX"
//  case class TmsClassDefQQ(pos: Position,
//                           symbol: Symbol,
//                           modifiers: Modifiers,
//                           typeName: TypeName,
//                           typeParameters: Seq[TypeDef],
//                           ctorModifiers: Modifiers,
//                           parameters: Seq[Seq[TmsValVarDef]],
//                           earlyValDefs: Seq[TmsValVarDef],
//                           parents: Seq[TmsExpression],
//                           selfValDef: TmsValVarDef,
//                           statements: Seq[TmsStatement]) extends TmsStatement {
//    lazy val dependsOnTts: Boolean = existsHasTts(parameters) || existsHasTts(earlyValDefs) || existsHasTts(parents) || existsHasTts(statements)
//  }

  // using QQ q"object ..." full pattern (TmsObjectDefQQ) as primary unlifter of "object O extends Parent(initExpression)" fails to unlift initExpression
  // when initExpression is implicitly converted class initializer value (like TmsClassDefQQ & TmsNewQQ fails)
  // Failed Tests samples (+ others TOOLBOX ones):
  // TmsForReachableSyntaxTest.scala:2357 "process object extending class creation with tts in extended class init"
//  case class TmsObjectDefQQ(pos: Position
//                            symbol: Symbol,
//                            modifiers: Modifiers,
//                            termName: TermName,
//                            earlyValDefs: Seq[TmsValVarDef],
//                            parents: Seq[TmsExpression],
//                            selfValDef: TmsValVarDef,
//                            statements: Seq[TmsStatement]) extends TmsStatement {
//    lazy val dependsOnTts: Boolean = existsHasTts(earlyValDefs) || existsHasTts(parents) || existsHasTts(statements)
//  }


  val unliftTmsExpressions: Seq[Tree] => Seq[TmsExpression] = (trees: Seq[Tree]) => trees.map(tree => {val q"${te: TmsExpression}" = tree; te})
  val unliftTmsStatements: Seq[Tree] => Seq[TmsStatement] = (trees: Seq[Tree]) => trees.map(tree => {val q"${ts: TmsStatement}" = tree; ts})
  val unliftTmsValVarDefs: Seq[Tree] => Seq[TmsValVarDef] = (trees: Seq[Tree]) => trees.map(tree => {val q"${tvvd: TmsValVarDef}" = tree; tvvd})
  @inline def unliftTmsValVarDefs(trees: Seq[Seq[Tree]]): Seq[Seq[TmsValVarDef]] = trees.map(unliftTmsValVarDefs) // don't drop: the usage is in the commented code

  private implicit val seqTmsExpressionTreePreprocessor:    TmsTreePreprocessor[Seq[TmsExpression]] = TmsTreePreprocessor.seqTreePreprocessor
  private implicit val seqSeqTmsExpressionTreePreprocessor: TmsTreePreprocessor[Seq[Seq[TmsExpression]]] = TmsTreePreprocessor.seqTreePreprocessor
  private implicit val seqTmsStatementTreePreprocessor:     TmsTreePreprocessor[Seq[TmsStatement]] = TmsTreePreprocessor.seqTreePreprocessor
  private implicit val seqTmsValVarDefTreePreprocessor:     TmsTreePreprocessor[Seq[TmsValVarDef]] = TmsTreePreprocessor.seqTreePreprocessor
  private implicit val seqSeqTmsValVarDefTreePreprocessor:  TmsTreePreprocessor[Seq[Seq[TmsValVarDef]]] = TmsTreePreprocessor.seqTreePreprocessor

  private implicit val seqTmsExpressionForExtractor:    TmsForExtractor[Seq[TmsExpression], Seq[Tree]] = TmsForExtractor.seqForExtractor
  private implicit val seqTmsStatementForExtractor:     TmsForExtractor[Seq[TmsStatement], Seq[Tree]] = TmsForExtractor.seqForExtractor
  private implicit val seqTmsValVarDefForExtractor:     TmsForExtractor[Seq[TmsValVarDef], Seq[Tree]] = TmsForExtractor.seqForExtractor
  private implicit val seqSeqTmsValVarDefForExtractor:  TmsForExtractor[Seq[Seq[TmsValVarDef]], Seq[Seq[Tree]]] = TmsForExtractor.seqForExtractor

  val existsHasTts: Seq[TmsStatement] => Boolean = (tmsStatements: Seq[TmsStatement]) => tmsStatements.exists(_.hasTts)
  @inline def existsHasTts(tmsStatements: Seq[Seq[TmsStatement]]): Boolean = tmsStatements.exists(existsHasTts)

  object TmsExpression {
    private val tmsExpressionUnlifter: PartialFunction[Tree, TmsExpression] = {
      case q"${tmsApply: TmsApply}" => tmsApply
      case q"${tmsFunction: TmsFunction}" => tmsFunction
      case q"${tmsCombinedExpr: TmsCombinedExpr}" => tmsCombinedExpr
      case q"${tmsBlock: TmsBlock}" => tmsBlock
      case q"${tmsNew: TmsNew}" => tmsNew // the TmsNewQQ && TmsBlock (includes TmsClassDef) order here influences the way how "new ..." in generated "new block" (class + new anon) is processed
      case q"${tmsAscription: TmsAscription}" => tmsAscription
      case q"${tmsOtherTree: TmsOtherTree}" => tmsOtherTree
    }
    implicit val tmsExpressionUnliftable: Unliftable[TmsExpression] =
      Unliftable[TmsExpression](tmsExpressionUnlifter.andThen(traceTree("Unlifted TmsExpression")(_)))

    implicit val tmsExpressionTreePreprocessor: TmsTreePreprocessor[TmsExpression] = new TmsTreePreprocessor[TmsExpression] {
      def preprocess(source: TmsExpression): TmsExpression = source match {
        case tmsApply: TmsApply => tmsApply.preprocess
        case tmsFunction: TmsFunction => tmsFunction.preprocess
        case tmsCombinedExpr: TmsCombinedExpr => tmsCombinedExpr.preprocess
        case tmsBlock: TmsBlock => tmsBlock.preprocess
        case tmsNew: TmsNew => tmsNew.preprocess
        case tmsAscription: TmsAscription => tmsAscription.preprocess
        case tmsOtherTree: TmsOtherTree => tmsOtherTree.preprocess
      }
    }

    implicit val tmsExpressionForExtractor: TmsForExtractor[TmsExpression, Tree] = new TmsForExtractor[TmsExpression, Tree] {
      def extract(source: TmsExpression, tmsForsBuilder: TmsForsBuilder)(implicit context: TmsExtractionContext): TmsForExtract[Tree] = source match {
        case tmsApply: TmsApply => tmsApply.extract(tmsForsBuilder)
        case tmsFunction: TmsFunction => tmsFunction.extract(tmsForsBuilder)
        case tmsCombinedExpr: TmsCombinedExpr => tmsCombinedExpr.extract(tmsForsBuilder)
        case tmsBlock: TmsBlock => tmsBlock.extract(tmsForsBuilder)
        case tmsNew: TmsNew => tmsNew.extract(tmsForsBuilder)
        case tmsAscription: TmsAscription => tmsAscription.extract(tmsForsBuilder)
        case tmsOtherTree: TmsOtherTree => tmsOtherTree.extract(tmsForsBuilder)
      }
    }
  }

  object TmsStatement {

    private val tmsStatementUnlifter: PartialFunction[Tree, TmsStatement] = {
      case q"${tmsValVarDef: TmsValVarDef}" => tmsValVarDef
      case q"${tmsDefDef: TmsDefDef}" => tmsDefDef
      case q"${tmsTraitDef: TmsTraitDefQQ}" => tmsTraitDef
      // TmsTraitDefQQ should be before TmsClassDef to correctly process traits first with q"" then ClassDef with Template
      // When ClassDef with Template is processed first (to process trait) then typechecker error occurs:
      // "no constructor in template" on instantiation of "new Trait {}" (without class).
      // But in that case constructor really exists with name $init$ instead of <init> one:
      // def /*TestTrait$1*/$init$(valueOfSome$macro$1$1: Int): Unit = ();
      case q"${tmsClassDef: TmsClassDef}" => tmsClassDef
      case q"${tmsModuleDef: TmsModuleDef}" => tmsModuleDef
      case q"${tmsExpression: TmsExpression}" => tmsExpression
    }
    implicit val tmsStatementUnliftable: Unliftable[TmsStatement] =
      Unliftable[TmsStatement](tmsStatementUnlifter.andThen(traceTree("Unlifted TmsStatement")(_)))

    implicit val tmsStatementTreePreprocessor: TmsTreePreprocessor[TmsStatement] = new TmsTreePreprocessor[TmsStatement] {
      def preprocess(source: TmsStatement): TmsStatement = source match {
        case tmsValVarDef: TmsValVarDef => tmsValVarDef.preprocess
        case tmsDefDef: TmsDefDef => tmsDefDef.preprocess
        case tmsTraitDef: TmsTraitDefQQ => tmsTraitDef.preprocess
        case tmsClassDef: TmsClassDef => tmsClassDef.preprocess
        case tmsModuleDef: TmsModuleDef => tmsModuleDef.preprocess
        case tmsExpression: TmsExpression => tmsExpression.preprocess
      }
    }

    implicit val tmsStatementForExtractor: TmsForExtractor[TmsStatement, Tree] = new TmsForExtractor[TmsStatement, Tree] {
      def extract(source: TmsStatement, tmsForsBuilder: TmsForsBuilder)(implicit context: TmsExtractionContext): TmsForExtract[Tree] = source match {
        case tmsValVarDef: TmsValVarDef => tmsValVarDef.extract(tmsForsBuilder)
        case tmsDefDef: TmsDefDef => tmsDefDef.extract(tmsForsBuilder)
        case tmsTraitDef: TmsTraitDefQQ => tmsTraitDef.extract(tmsForsBuilder)
        case tmsClassDef: TmsClassDef => tmsClassDef.extract(tmsForsBuilder)
        case tmsModuleDef: TmsModuleDef => tmsModuleDef.extract(tmsForsBuilder)
        case tmsExpression: TmsExpression => tmsExpression.extract(tmsForsBuilder)
      }
    }
  }

  object TmsApply {

    private def applyIsTts(applySourceTmsExpression: TmsExpression, applyTermName: TermName): Boolean =
      applySourceTmsExpression match {
        case applySourceOtherTree: TmsOtherTree =>
          applyTermName.toString.startsWith("tts") && applySourceOtherTree.tree.toString == ttsImplicitsHolderPackage
        case _ => false
      }

    implicit val tmsApplyUnliftable: Unliftable[TmsApply] = Unliftable[TmsApply] {
      case tree@q"${tmsExpression: TmsExpression}.${termName: TermName}[..${types: Seq[Tree]}](...${arguments: Seq[Seq[TmsExpression]]})"
        if applyIsTts(tmsExpression, termName) || tmsExpression.hasTts || existsHasTts(arguments) => // should skip to do not produce TmsApply for each qualifier of the chain of Selects like sands.sugar.tms....
        TmsApply(tree.pos, tree.tpe, Some(tmsExpression), termName, None, types, arguments)

      case tree@q"${termName: TermName}[..${types: Seq[Tree]}](...${arguments: Seq[Seq[TmsExpression]]})"
        if existsHasTts(arguments) => // should skip to do not produce TmsApply for the first qualifier of the chain of Selects like _sands_.sugar.tms....
        TmsApply(tree.pos, tree.tpe, None, termName, findApplyIdentSymbol(tree), types, arguments)

      case tree@q"${tq"${tName: Tree}[..${tArgs: Seq[Tree]}]"}(...${arguments: Seq[Seq[TmsExpression]]})" => // applied type (parent in new Class[Int](1)(2))
        // TmsApply.tpe (tree.tpe) is null here: looks like such TmsApply is used only as part of "new ..parents" and its tpe is never accessed
        TmsApply(tree.pos, tree.tpe, None, TermName(""), None, Seq(tName) ++ tArgs, arguments)
    }

    private def findApplyIdentSymbol(tree: Tree): Option[Symbol] = tree match {
      case Apply(ident@Ident(_: TermName), _) => Some(ident.symbol)
      case _ => None
    }

    private val replaceTtsByAddStringValueOf: TmsApply => TmsApply = tmsApply =>
      if (tmsOptions.predefCompliance) {
        val patchedMaybeTermSource = tmsApply.maybeTermSource.map { tmsApplySource =>
          tmsApplySource.nonAscripted match {
            case tmsApplyNonAscriptedSourceTts: TmsApply if tmsApplyNonAscriptedSourceTts.isTts => // isTts == true: replace ttsN by any2stringadd(sourceTts.expression) for implicit Predef.any2stringadd like behaviour
              TmsApply(tmsApplyNonAscriptedSourceTts.pos, typeOf[String], None, TermName("any2stringadd"), None, Seq(), Seq(Seq(tmsApplyNonAscriptedSourceTts.firstArgument)))
            case _ =>
              tmsApplySource
          }
        }
        tmsApply.copy(maybeTermSource = patchedMaybeTermSource)
      } else
        tmsApply

    private val dropApplyTypeArguments: TmsApply => TmsApply = tmsApply =>
      tmsApply.copy(types = Seq())

    private case class TmsApplyPatchPattern(maybeTtsSourceBaseType: Option[Type], termName: String, maybeArgumentBaseTypes: Option[Seq[Seq[Type]]], patched: TmsApply => TmsApply) {
      def matches(tmsApply: TmsApply): Boolean =
        tmsApply.termName.toString == termName &&
          maybeTtsSourceBaseType.forall(bt => tmsApply.maybeTermSource.exists(_.tpe <:< bt)) &&
          maybeArgumentBaseTypes.forall { btss =>
            tmsApply.arguments.map(_.length) == btss.map(_.length) &&
              tmsApply.arguments.flatten.zip(btss.flatten).forall { case (ae, bt) => ae.tpe <:< bt }
          }
    }

    private val someTypeOfString: Some[Type] = Some(typeOf[String])

    // valid for Java 16 inclusive
    private val tmsApplyPatchPatterns = Seq(
      TmsApplyPatchPattern(None, "$plus", Some(Seq(Seq(typeOf[String]))), replaceTtsByAddStringValueOf),
      TmsApplyPatchPattern(someTypeOfString, "contains",    None, dropApplyTypeArguments),
      TmsApplyPatchPattern(someTypeOfString, "startsWith",  None, dropApplyTypeArguments),
      TmsApplyPatchPattern(someTypeOfString, "endsWith",    None, dropApplyTypeArguments),
      TmsApplyPatchPattern(someTypeOfString, "indexOf",     None, dropApplyTypeArguments),
      TmsApplyPatchPattern(someTypeOfString, "lastIndexOf", None, dropApplyTypeArguments),
      TmsApplyPatchPattern(someTypeOfString, "concat",      None, dropApplyTypeArguments) // since 2.13: for prev versions it's ok: nothing to drop before 2.13
    )

    private def patchApplyWithTtsSourceOnPattern(tmsApply: TmsApply): TmsApply =
      tmsApply.maybeTermSource.fold(tmsApply) { tmsApplySource =>
        tmsApplySource.nonAscripted match {
          case tmsApplyNonAscriptedSource: TmsApply if tmsApplyNonAscriptedSource.isTts => // non-ascripted source of this tmsApply is Tts: patch if it matches pattern
            tmsApplyPatchPatterns.find(_.matches(tmsApply))
              .fold(tmsApply)(_.patched(tmsApply))
          case _ =>
            tmsApply
        }
      }

    implicit val tmsApplyTreePreprocessor: TmsTreePreprocessor[TmsApply] = new TmsTreePreprocessor[TmsApply] {
      def preprocess(tmsApply: TmsApply): TmsApply = {
        val preprocessedTmsApply = if (tmsApply.isTts) flattenNestedTts(tmsApply) else patchApplyWithTtsSourceOnPattern(tmsApply)

        preprocessedTmsApply.copy(
          maybeTermSource = preprocessedTmsApply.maybeTermSource.map(_.preprocess),
          arguments = preprocessedTmsApply.arguments.preprocess
        )
      }
    }

    private def flattenNestedTts(outer: TmsApply): TmsApply =
      outer.firstArgument.nonAscripted match {
        case inner: TmsApply if inner.isTts => // Merges nested ttsNOuter(ttsNInner(..)) to single one skipping possible Ascriptions between them
          TmsApply(outer.pos, outer.tpe, outer.maybeTermSource, outer.termName, outer.maybeIdentSymbol, inner.types.init ++ outer.types, Seq(Seq(inner.firstArgument)))
        case _ =>
          outer
      }

    /**
     * Extracts TmsExpression as new Inner Fors Stack if it will not be degenerate
     * (only when expression contains ttsN operations: not just single, possibly ascripted, but ttsN with .apply on it or
     * with expression that depends on another ttsM, so the yield of the built Inner Fors Stack will be a real expression, not just an ident)
     */
    private val tmsExpressionAsInnerForsStackForExtractor: TmsForExtractor[TmsExpression, Tree] = new TmsForExtractor[TmsExpression, Tree]  {
      def extract(tmsExpression: TmsExpression, tmsForsBuilder: TmsForsBuilder)(implicit context: TmsExtractionContext): TmsForExtract[Tree] = tmsExpression match {
        // seq ': _*' ascription of passed parameter expression should stay in the place of parameter call, it may not be moved to value extraction site and come to parameter "with" extracted value
        case tmsAscription: TmsAscription => // skip new fors stack: extract tmsAscription.expression using this Inner Fors Stack extractor (to see what is "inside" ascription: does it need Inner Fors Stack)
          val TmsForExtract(extractedExpressionForsBuilder, extractedExpression) =
            extractWithMaxOfReservedTypes(tmsExpressionAsInnerForsStackForExtractor, tmsAscription.expression, tmsForsBuilder)
          TmsForExtract(extractedExpressionForsBuilder, q"$extractedExpression : ${tmsAscription.ascription}")

        // only when expression dependsOnTts. Non-dependent expressions and "clean" tts that does not depend on other tts are skipped: one does not require separate Fors Stack (it will be degenerate if built)
        case expr if expr.dependsOnTts => // build new fors stack: this non-tts apply depends on tts or this tts-apply depends on other tts
          traceExtraction("tmsExpression that dependsOnTts")(tmsExpression)

          val innerForsBuilder = traceExtraction("Building inner Fors Stack in new builder") {
            tmsForsBuilder.reset // copies forsBuilder with reset ForCollectors & 0 reserved types to have new forsBuilder for the same types stack
          }

          // innerTmsExtractionContext:
          // 1) inner Fors Stacks never have Monadic Flow (but if Mfc Type is present then it is used to detect if collector of inner Fors Stack requires pre evaluation)
          // 2) mfVals are always non-local defs for inner Fors Stack and will not be scope verified during this inner extraction,
          // but will be scope verified when this inner (including children) Fors Stack is added to parent root Fors Stack
          // (scope correctness depends on root monadic Flow Fors Stack type-level where this inner should be added)
          val innerTmsExtractionContext = TmsExtractionContext.withoutMonadicFlow(context.nonMfValDefSymbols, context.mfBaseType)
          val innerTmsForExtract = tmsExpression.extract(innerForsBuilder)(innerTmsExtractionContext) // extract expression to new innerForsBuilder by regular tmsExpressionForExtractor to be built in the inner fors stack
          val innerTmsExtractedForsStack = TmsExtractedForsStack(innerTmsForExtract.forsBuilder, innerTmsForExtract.extract)
          val innerForsStackUsedTypes = innerTmsForExtract.forsBuilder.usedTypes // gets types of non-empty forCollectors
          val innerValueTmsForExtract = tmsForsBuilder.withResetReservedTypes.extractMonadicValue(innerForsStackUsedTypes, innerTmsExtractedForsStack, tmsExpression) // add it to parent tmsForsBuilder (inner for = builder & it's yield = tmsExtract) to outer `for` stack like TmsApply extractor does

          TmsForExtract[Tree](innerValueTmsForExtract.forsBuilder.withMaxOfReservedTypes(tmsForsBuilder), innerValueTmsForExtract.extract)

        case _ => // bypass new fors stack creation: neither non-tts apply nor argument of tts apply contains another tts inside - extract it by usual extractor "leaving" this extractor
          tmsExpression.extract(tmsForsBuilder)
      }
    }

    implicit val tmsApplyForExtractor: TmsForExtractor[TmsApply, Tree] = new TmsForExtractor[TmsApply, Tree] {
      private implicit val seqSeqArgumentForExtractor: TmsForExtractor[Seq[Seq[TmsExpression]], Seq[Seq[Tree]]] =
        if (tmsOptions.forsStackForApplyParameter)
          TmsForExtractor.seqForExtractor(TmsForExtractor.seqForExtractor(tmsExpressionAsInnerForsStackForExtractor))
        else
          TmsForExtractor.seqForExtractor(TmsForExtractor.seqForExtractor(TmsExpression.tmsExpressionForExtractor))

      def extract(tmsApply: TmsApply, tmsForsBuilder: TmsForsBuilder)(implicit context: TmsExtractionContext): TmsForExtract[Tree] = tmsApply match {
        case tmsApply@TmsApply(_, _, Some(termSource), termName, _, types, arguments) if !tmsApply.isTts => // real .apply, not ttsNxxx
          val TmsForExtract(extractedSourceForsBuilder, extractedSource) = if (tmsOptions.forsStackForApplySource)
            extractWithMaxOfReservedTypes(tmsExpressionAsInnerForsStackForExtractor, termSource, tmsForsBuilder)
          else
            extractWithMaxOfReservedTypes(TmsExpression.tmsExpressionForExtractor, termSource, tmsForsBuilder)

          val untypecheckedTypes = types.map(c.untypecheck)
          val TmsForExtract(extractedArgumentsForsBuilder, extractedArguments) = arguments.extract(extractedSourceForsBuilder)

          TmsForExtract(extractedArgumentsForsBuilder, q"$extractedSource.$termName[..$untypecheckedTypes](...$extractedArguments)")

        case tmsApplyTts@TmsApply(_, _, Some(_), _, _, types, _) => // tmsApplyTts.isTts == true
          val resetReservedTypesForsBuilder = tmsForsBuilder.withResetReservedTypes
          val TmsForExtract(extractedForsBuilder, extractedArgumentTree) =
            extractWithMaxOfReservedTypes(TmsExpression.tmsExpressionForExtractor, tmsApplyTts.firstArgument, resetReservedTypesForsBuilder) // always independently of Options: extracted without new Fors Stack

          val ttsTypes: Seq[Type] = types.init.map(_.tpe)
          traceExtraction(s"Building Fors Stack monadic value extraction for tts types ${formattedTypeNames(ttsTypes)} of expression")(extractedArgumentTree.toString)

          extractedForsBuilder.extractMonadicValue(ttsTypes, TmsExtractedTree(extractedArgumentTree), tmsApplyTts.firstArgument)

        case TmsApply(_, _, None, termName, maybeIdentSymbol, types, arguments) =>
          val untypecheckedTypes = types.map(c.untypecheck)
          val TmsForExtract(extractedArgumentsForsBuilder, extractedArguments) = arguments.extract(tmsForsBuilder)
          val liftedTree = if (termName.toString == "")
            q"${tq"${untypecheckedTypes.head}[..${untypecheckedTypes.tail}]"}(...$extractedArguments)" // applied type (parent in new Class[Int](1)(2) & Template)
          else {
            val ident = Ident(termName)
            maybeIdentSymbol.foreach(symbol => c.internal.setSymbol(ident, symbol))
            q"$ident[..$untypecheckedTypes](...$extractedArguments)"
          }

          TmsForExtract(extractedArgumentsForsBuilder, liftedTree)
      }
    }
  }

  object TmsFunction {
    implicit val tmsFunctionUnliftable: Unliftable[TmsFunction] = Unliftable[TmsFunction] {
      case tree@q"(..${params: Seq[Tree]}) => ${body: TmsExpression}" =>
        TmsFunction(tree.pos, tree.tpe, unliftTmsValVarDefs(params), body)
    }

    implicit val tmsFunctionTreePreprocessor: TmsTreePreprocessor[TmsFunction] = new TmsTreePreprocessor[TmsFunction] {
      def preprocess(tmsFunction: TmsFunction): TmsFunction =
        tmsFunction.copy(
          params = tmsFunction.params.preprocess, // for the generality, it does nothing
          body = tmsFunction.body.preprocess
        )
    }

    implicit val tmsFunctionForExtractor: TmsForExtractor[TmsFunction, Tree] = new TmsForExtractor[TmsFunction, Tree] {
      def extract(tmsFunction: TmsFunction, tmsForsBuilder: TmsForsBuilder)(implicit context: TmsExtractionContext): TmsForExtract[Tree] = {
        import tmsFunction._
        val TmsForExtract(extractedParamsForsBuilder, extractedParams) = params.extract(tmsForsBuilder) // just lifting, no default values
        val TmsForExtract(extractedFunctionForsBuilder, extractedBody) = body.extract(extractedParamsForsBuilder)
        TmsForExtract(extractedFunctionForsBuilder, q"(..$extractedParams) => $extractedBody")
      }
    }
  }

  object TmsCombinedExpr {

    sealed trait CombinedExprType

    case object CeIfType extends CombinedExprType
    case object CePartFunctionType extends CombinedExprType
    case object CeMatchType extends CombinedExprType
    case object CeTryType extends CombinedExprType
    case object CeWhileType extends CombinedExprType
    case object CeDoWhileType extends CombinedExprType
    case object CeAssignmentType extends CombinedExprType
    case object CeReturnType extends CombinedExprType

    implicit val tmsCombinedExprUnliftable: Unliftable[TmsCombinedExpr] = Unliftable[TmsCombinedExpr] {

      case tree@q"if (${predicate: TmsExpression}) ${thenExpression: TmsExpression} else ${elseExpression: TmsExpression}" =>
        TmsCombinedExpr(tree.pos, tree.tpe, CeIfType, Seq(Seq(predicate, thenExpression, elseExpression)))

      case tree@q"{ case ..${cases: Seq[CaseDef]} }" =>
        TmsCombinedExpr(tree.pos, tree.tpe, CePartFunctionType, unliftCaseDefs(cases))

      case tree@q"${expression: TmsExpression} match { case ..${cases: Seq[CaseDef]} }" =>
        TmsCombinedExpr(tree.pos, tree.tpe, CeMatchType, Seq(Seq(expression)) ++ unliftCaseDefs(cases))

      case tree@q"try ${expression: TmsExpression} catch { case ..${cases: Seq[CaseDef]} } finally ${finallyExpression: TmsExpression}" =>
        TmsCombinedExpr(tree.pos, tree.tpe, CeTryType, Seq(Seq(expression)) ++ unliftCaseDefs(cases) ++ Seq(Seq(finallyExpression)))

      case tree@q"while (${predicate: TmsExpression}) ${bodyExpression: TmsExpression}" =>
        TmsCombinedExpr(tree.pos, tree.tpe, CeWhileType, Seq(Seq(predicate, bodyExpression)))

      case tree@q"do ${bodyExpression: TmsExpression} while (${predicate: TmsExpression})" =>
        TmsCombinedExpr(tree.pos, tree.tpe, CeDoWhileType, Seq(Seq(bodyExpression, predicate)))

      case tree@q"${leftExpression: TmsExpression} = ${rightExpression: TmsExpression}" =>
        TmsCombinedExpr(tree.pos, tree.tpe, CeAssignmentType, Seq(Seq(leftExpression, rightExpression)))

      case tree@q"return ${returnExpression: TmsExpression}" =>
        TmsCombinedExpr(tree.pos, tree.tpe, CeReturnType, Seq(Seq(returnExpression)))

    }

    private def unliftCaseDefs(cases: Seq[CaseDef]): Seq[Seq[TmsExpression]] =
      cases.map {
        case CaseDef(q"${patOtherTree: TmsOtherTree}", q"${guardExpression: TmsExpression}", q"${bodyExpression: TmsExpression}") =>
          Seq(patOtherTree.copy(untypecheck = false), guardExpression, bodyExpression)
      }

    implicit val tmsCombinedExprTreePreprocessor: TmsTreePreprocessor[TmsCombinedExpr] = new TmsTreePreprocessor[TmsCombinedExpr] {
      def preprocess(tmsCombinedExpr: TmsCombinedExpr): TmsCombinedExpr =
        tmsCombinedExpr.copy(expressions = tmsCombinedExpr.expressions.preprocess)
    }

    implicit val tmsCombinedExprForExtractor: TmsForExtractor[TmsCombinedExpr, Tree] = new TmsForExtractor[TmsCombinedExpr, Tree] {
      private implicit val seqSeqTmsExpressionForExtractor: TmsForExtractor[Seq[Seq[TmsExpression]], Seq[Seq[Tree]]] = TmsForExtractor.seqForExtractor

      def extract(tmsCombinedExpr: TmsCombinedExpr, tmsForsBuilder: TmsForsBuilder)(implicit context: TmsExtractionContext): TmsForExtract[Tree] = tmsCombinedExpr match {
        case TmsCombinedExpr(_, _, ceType, expressions) =>
          val TmsForExtract(extractedArgumentsForsBuilder, extractedArguments) = expressions.extract(tmsForsBuilder)
          val tree = ceType match {
            case CeIfType =>
              val Seq(Seq(predicateExpression, thenExpression, elseExpression)) = extractedArguments
              q"if ($predicateExpression) $thenExpression else $elseExpression"

            case CePartFunctionType =>
              val cases: Seq[CaseDef] = liftCaseDefs(extractedArguments)
              q"{ case ..$cases }"

            case CeMatchType =>
              val Seq(expression) = extractedArguments.head
              val cases: Seq[CaseDef] = liftCaseDefs(extractedArguments.tail)
              q"$expression match { case ..$cases }"

            case CeTryType =>
              val Seq(expression) = extractedArguments.head
              val cases: Seq[CaseDef] = liftCaseDefs(extractedArguments.tail.init)
              val Seq(finallyExpression) = extractedArguments.last
              q"try $expression catch { case ..$cases } finally $finallyExpression"

            case CeWhileType =>
              val Seq(Seq(predicateExpression, bodyExpression)) = extractedArguments
              q"while($predicateExpression) $bodyExpression"

            case CeDoWhileType =>
              val Seq(Seq(bodyExpression, predicateExpression)) = extractedArguments
              q"do $bodyExpression while($predicateExpression)"

            case CeAssignmentType =>
              val Seq(Seq(leftExpression, rightExpression)) = extractedArguments
              q"$leftExpression = $rightExpression"

            case CeReturnType =>
              val Seq(Seq(returnExpression)) = extractedArguments
              q"return $returnExpression"
          }
          TmsForExtract(extractedArgumentsForsBuilder, tree)
      }
    }

    private def liftCaseDefs(cases: Seq[Seq[Tree]]): Seq[CaseDef] =
      cases.map {
        case Seq(patOtherTree, guardExpression, bodyExpression) =>
          CaseDef(patOtherTree, guardExpression, bodyExpression)
      }
  }

  object TmsBlock {

    implicit val tmsBlockUnliftable: Unliftable[TmsBlock] = Unliftable[TmsBlock] {
      // matching on block quasiquotes q"{..$stats}" results in StackOverflow at one TmsOtherTree statement
      case tree@Block(_, _) =>
        val q"{ ..${blockStatements: Seq[TmsStatement]} }" = tree
        TmsBlock(tree.pos, tree.tpe, blockStatements)
    }

    implicit val tmsBlockTreePreprocessor: TmsTreePreprocessor[TmsBlock] = new TmsTreePreprocessor[TmsBlock] {
      def preprocess(source: TmsBlock): TmsBlock =
        source.copy(statements = source.statements.preprocess)
    }

    implicit val tmsBlockForExtractor: TmsForExtractor[TmsBlock, Tree] = new TmsForExtractor[TmsBlock, Tree] {
      def extract(tmsBlock: TmsBlock, tmsForsBuilder: TmsForsBuilder)(implicit context: TmsExtractionContext): TmsForExtract[Tree] = {
        val TmsForExtract(extractedBlockForsBuilder, extractedStatements) = tmsBlock.statements.extract(tmsForsBuilder)
        TmsForExtract(extractedBlockForsBuilder, q"{ ..$extractedStatements }")
      }
    }

  }

  object TmsNew {

    implicit val tmsNewUnliftable: Unliftable[TmsNew] = Unliftable[TmsNew] {
      case tree@q"new ${appliedTypeExpression: TmsExpression}" =>
        TmsNew(tree.pos, tree.tpe, appliedTypeExpression)
    }

    implicit val tmsNewTreePreprocessor: TmsTreePreprocessor[TmsNew] = new TmsTreePreprocessor[TmsNew] {
      def preprocess(source: TmsNew): TmsNew = source.copy(
        appliedTypeExpression = source.appliedTypeExpression.preprocess
      )
    }

    implicit val tmsNewForExtractor: TmsForExtractor[TmsNew, Tree] = new TmsForExtractor[TmsNew, Tree] {
      def extract(tmsNew: TmsNew, tmsForsBuilder: TmsForsBuilder)(implicit context: TmsExtractionContext): TmsForExtract[Tree] = {
        val TmsForExtract(extractedNewForsBuilder, extractedAppliedTypeExpression) = tmsNew.appliedTypeExpression.extract(tmsForsBuilder)
        TmsForExtract(extractedNewForsBuilder, q"new $extractedAppliedTypeExpression")
      }
    }
  }

  // see class TmsNewQQ comments
//  object TmsNewQQ {
//
//    implicit val tmsNewInstanceUnliftable: Unliftable[TmsNewQQ] = Unliftable[TmsNewQQ] {
//      case tree@q"new { ..${earlydefns: Seq[Tree]} } with ..${parents: Seq[Tree]} { ${self: TmsValVarDef} => ..${stats: Seq[TmsStatement]} }" =>
//        TmsNewQQ(tree.pos, tree.tpe, unliftTmsValVarDefs(earlydefns), unliftTmsExpressions(parents), self, stats)
//    }
//
//    implicit val tmsNewInstanceTreePreprocessor: TmsTreePreprocessor[TmsNewQQ] = new TmsTreePreprocessor[TmsNewQQ] {
//      def preprocess(source: TmsNewQQ): TmsNewQQ = source.copy(
//        earlyValDefs = source.earlyValDefs.preprocess,
//        parents = source.parents.preprocess,
//        // nothing to preprocess in selfValDef
//        statements = source.statements.preprocess
//      )
//    }
//
//    implicit val tmsNewInstanceForExtractor: TmsForExtractor[TmsNewQQ, Tree] = new TmsForExtractor[TmsNewQQ, Tree] {
//      def extract(tmsNewInstance: TmsNewQQ, tmsForsBuilder: TmsForsBuilder): TmsForExtract[Tree] = {
//        import tmsNewInstance._
//        val TmsForExtract(extractedEarlyValDefsForsBuilder, extractedEarlyValDefs) = earlyValDefs.extract(tmsForsBuilder)
//        val TmsForExtract(extractedParentsForsBuilder, extractedParentExpressions) = parents.extract(extractedEarlyValDefsForsBuilder)
//        val TmsForExtract(extractedSelfForsBuilder, extractedSelf) = selfValDef.extract(extractedParentsForsBuilder)
//        val TmsForExtract(extractedNewInstanceForsBuilder, extractedStatements) = statements.extract(extractedSelfForsBuilder)
//        TmsForExtract(extractedNewInstanceForsBuilder, q"new { ..$extractedEarlyValDefs } with ..$extractedParentExpressions { $extractedSelf => ..$extractedStatements }")
//      }
//    }
//  }

  object TmsAscription {

    implicit val tmsAscriptionUnliftable: Unliftable[TmsAscription] = Unliftable[TmsAscription] {
      case tree@q"${expression: TmsExpression}: ${ascription: Tree}" =>
        traceTree("Unlifted ascription")(TmsAscription(tree.pos, tree.tpe, expression, ascription))
      case tree@q"${expression: TmsExpression}: @${annotation: Tree}" =>
        traceTree("Unlifted annotation")(TmsAscription(tree.pos, tree.tpe, expression, annotation))
    }

    implicit val tmsAscriptionTreePreprocessor: TmsTreePreprocessor[TmsAscription] = new TmsTreePreprocessor[TmsAscription] {
      def preprocess(source: TmsAscription): TmsAscription =
        source.copy(expression = source.expression.preprocess)
    }

    implicit val tmsAscriptionForExtractor: TmsForExtractor[TmsAscription, Tree] = new TmsForExtractor[TmsAscription, Tree] {
      def extract(tmsAscription: TmsAscription, tmsForsBuilder: TmsForsBuilder)(implicit context: TmsExtractionContext): TmsForExtract[Tree] = {
        val TmsForExtract(extractedExpressionForsBuilder, extractedExpression) = tmsAscription.expression.extract(tmsForsBuilder)
        TmsForExtract(extractedExpressionForsBuilder, q"$extractedExpression : ${tmsAscription.ascription}") // tmsAscription.ascription should not be untypechecked !!!
      }
    }
  }

  object TmsOtherTree {

    implicit val tmsOtherTreeUnliftable: Unliftable[TmsOtherTree] = Unliftable[TmsOtherTree] {
      case tree: Tree => TmsOtherTree(tree)
    }

    implicit val tmsOtherTreeTreePreprocessor: TmsTreePreprocessor[TmsOtherTree] = new TmsTreePreprocessor[TmsOtherTree] {
      def preprocess(source: TmsOtherTree): TmsOtherTree = source
    }

    implicit val tmsOtherTreeForExtractor: TmsForExtractor[TmsOtherTree, Tree] = new TmsForExtractor[TmsOtherTree, Tree] {
      def extract(source: TmsOtherTree, tmsForsBuilder: TmsForsBuilder)(implicit context: TmsExtractionContext): TmsForExtract[Tree] = {
        TmsForExtract(tmsForsBuilder, if (source.untypecheck & !source.tree.isDef) c.untypecheck(source.tree) else source.tree) // type def will be untypechecked in UntypecheckTransformer. Before: it is used to verify definitions scope
      }
    }
  }

  object TmsValVarDef {

    implicit val tmsValVarDefUnliftable: Unliftable[TmsValVarDef] = Unliftable[TmsValVarDef] {
      case tree@q"${mods: Modifiers} val ${name: TermName}: ${tpt: Tree} = ${rhs: TmsExpression}" =>
        TmsValVarDef(tree.pos, isVal = true, tree.symbol, mods, name, tpt, rhs)
      case tree@q"${mods: Modifiers} var ${name: TermName}: ${tpt: Tree} = ${rhs: TmsExpression}" =>
        TmsValVarDef(tree.pos, isVal = false, tree.symbol, mods, name, tpt, rhs)
    }

    implicit val tmsValVarDefTreePreprocessor: TmsTreePreprocessor[TmsValVarDef] = new TmsTreePreprocessor[TmsValVarDef] {
      def preprocess(source: TmsValVarDef): TmsValVarDef =
        source.copy(initializer = source.initializer.preprocess)
    }

    implicit val tmsValVarDefForExtractor: TmsForExtractor[TmsValVarDef, Tree] = new TmsForExtractor[TmsValVarDef, Tree] {
      def extract(tmsValVarDef: TmsValVarDef, tmsForsBuilder: TmsForsBuilder)(implicit context: TmsExtractionContext): TmsForExtract[Tree] = {
        import tmsValVarDef._
        val TmsForExtract(extractedValVarDefForsBuilder, extractedInitializer) = initializer.extract(tmsForsBuilder)
        //TO-DO drop PRESUPER in 3.0
        // if to untypecheck tpe of early val (with Flag.PRESUPER) then final (post-macros) typechecker fails with (on `val e: Int` where Int tree becomes not a TypeTree!)
        //[error] scala.MatchError: (val e: Int = _,val e: scala.Int = valueOfSome$macro$2) (of class scala.Tuple2)
        //[error] scala.reflect.internal.ReificationSupport$ReificationSupportImpl$UnMkTemplate.$anonfun$unapply$5(ReificationSupport.scala:304)
        // It's fun that this error does not happen in TOOLBOX (only in real code tests)
        val untypecheckedTpe = if (!modifiers.hasFlag(Flag.PRESUPER)) c.untypecheck(tpt) else tpt
        val liftedTree = if (isVal)
          q"$modifiers val $name: $untypecheckedTpe = $extractedInitializer"
        else
          q"$modifiers var $name: $untypecheckedTpe = $extractedInitializer"

        c.internal.setSymbol(liftedTree, symbol)
        TmsForExtract(extractedValVarDefForsBuilder, liftedTree)
      }
    }
  }

  object TmsDefDef {

    implicit val tmsDefDefUnliftable: Unliftable[TmsDefDef] = Unliftable[TmsDefDef] {
      case tree@q"${mods: Modifiers} def ${name: TermName}[..${tparams: Seq[TypeDef]}](...${paramss: Seq[Seq[TmsValVarDef]]}): ${tpt: Tree} = ${body: TmsExpression}" =>
        TmsDefDef(tree.pos, tree.symbol, mods, name, tparams, paramss, tpt, body)
    }

    implicit val tmsDefDefTreePreprocessor: TmsTreePreprocessor[TmsDefDef] = new TmsTreePreprocessor[TmsDefDef] {
      def preprocess(source: TmsDefDef): TmsDefDef = source.copy(
        parameters = source.parameters.preprocess,
        body = source.body.preprocess
      )
    }

    implicit val tmsDefDefForExtractor: TmsForExtractor[TmsDefDef, Tree] = new TmsForExtractor[TmsDefDef, Tree] {
      def extract(tmsDefDef: TmsDefDef, tmsForsBuilder: TmsForsBuilder)(implicit context: TmsExtractionContext): TmsForExtract[Tree] = {
        import tmsDefDef._
        val TmsForExtract(extractedParametersForsBuilder, extractedParameters) = parameters.extract(tmsForsBuilder)
        val TmsForExtract(extractedDefDefForsBuilder, extractedBody) = body.extract(extractedParametersForsBuilder)
        val untypecheckedReturnType = c.untypecheck(returnType) // !!! should be done for typed def, class, trait instantiation
        val liftedTree = q"$modifiers def $name[..$typeParameters](...$extractedParameters): $untypecheckedReturnType = $extractedBody"
        c.internal.setSymbol(liftedTree, symbol)
        TmsForExtract(extractedDefDefForsBuilder, liftedTree)
      }
    }
  }

  object TmsClassDef {

    implicit val tmsClassDefUnliftable: Unliftable[TmsClassDef] = Unliftable[TmsClassDef] {
      case tree@ClassDef(modifiers: Modifiers, typeName: TypeName, typeParameters: Seq[TypeDef], q"${template: TmsTemplate}") =>
        TmsClassDef(tree.pos, tree.symbol, modifiers, typeName, typeParameters, template)
    }

    implicit val tmsClassDefTreePreprocessor: TmsTreePreprocessor[TmsClassDef] = new TmsTreePreprocessor[TmsClassDef] {
      def preprocess(source: TmsClassDef): TmsClassDef = source.copy(
        template = source.template.preprocess
      )
    }

    implicit val tmsClassDefForExtractor: TmsForExtractor[TmsClassDef, Tree] = new TmsForExtractor[TmsClassDef, Tree] {
      def extract(tmsClassDef: TmsClassDef, tmsForsBuilder: TmsForsBuilder)(implicit context: TmsExtractionContext): TmsForExtract[Tree] = {
        import tmsClassDef._
        val TmsForExtract(extractedClassDefForsBuilder, extractedTemplate) = template.extract(tmsForsBuilder)
        val liftedTree = ClassDef(modifiers, typeName, typeParameters.toList, extractedTemplate)
        c.internal.setSymbol(liftedTree, symbol)
        TmsForExtract(extractedClassDefForsBuilder, liftedTree)
      }
    }
  }

  // object
  object TmsModuleDef {

    implicit val tmsModuleDefUnliftable: Unliftable[TmsModuleDef] = Unliftable[TmsModuleDef] {
      case tree@ModuleDef(modifiers: Modifiers, termName: TermName, q"${template: TmsTemplate}") =>
        TmsModuleDef(tree.pos, tree.symbol, modifiers, termName, template)
    }

    implicit val tmsModuleDefTreePreprocessor: TmsTreePreprocessor[TmsModuleDef] = new TmsTreePreprocessor[TmsModuleDef] {
      def preprocess(source: TmsModuleDef): TmsModuleDef = source.copy(
        template = source.template.preprocess
      )
    }

    implicit val tmsModuleDefForExtractor: TmsForExtractor[TmsModuleDef, Tree] = new TmsForExtractor[TmsModuleDef, Tree] {
      def extract(tmsModuleDef: TmsModuleDef, tmsForsBuilder: TmsForsBuilder)(implicit context: TmsExtractionContext): TmsForExtract[Tree] = {
        import tmsModuleDef._
        val TmsForExtract(extractedObjectDefForsBuilder, extractedTemplate) = template.extract(tmsForsBuilder)
        val liftedTree = ModuleDef(modifiers, termName, extractedTemplate)
        c.internal.setSymbol(liftedTree, symbol)
        TmsForExtract(extractedObjectDefForsBuilder, liftedTree)
      }
    }
  }

  object TmsTemplate {

    implicit val tmsTemplateUnliftable: Unliftable[TmsTemplate] = Unliftable[TmsTemplate] {
      case Template(parents: Seq[Tree], q"${selfValDef: TmsValVarDef}", statements: Seq[Tree]) =>
        TmsTemplate(unliftTmsExpressions(parents), selfValDef, unliftTmsStatements(statements))
    }

    implicit val tmsTemplateTreePreprocessor: TmsTreePreprocessor[TmsTemplate] = new TmsTreePreprocessor[TmsTemplate] {
      def preprocess(source: TmsTemplate): TmsTemplate = source.copy(
        parents = source.parents.preprocess,
        // nothing to preprocess in selfValDef
        statements = source.statements.preprocess
      )
    }

    implicit val tmsTemplateForExtractor: TmsForExtractor[TmsTemplate, Template] = new TmsForExtractor[TmsTemplate, Template] {
      def extract(tmsTemplate: TmsTemplate, tmsForsBuilder: TmsForsBuilder)(implicit context: TmsExtractionContext): TmsForExtract[Template] = {
        import tmsTemplate._
        val TmsForExtract(extractedParentsForsBuilder, extractedParents) = parents.extract(tmsForsBuilder)
        val TmsForExtract(extractedSelfForsBuilder, extractedSelf) = selfValDef.extract(extractedParentsForsBuilder)
        val TmsForExtract(extractedTemplateForsBuilder, extractedStatements) = statements.extract(extractedSelfForsBuilder)
        TmsForExtract(
          extractedTemplateForsBuilder,
          Template(extractedParents.toList, extractedSelf.asInstanceOf[ValDef], extractedStatements.toList)
        )
      }
    }
  }

  object TmsTraitDefQQ {

    implicit val tmsTraitDefUnliftable: Unliftable[TmsTraitDefQQ] = Unliftable[TmsTraitDefQQ] {
      case tree@q"${mods: Modifiers} trait ${tName: TypeName}[..${tparams: Seq[TypeDef]}] extends { ..${earlydefns: Seq[Tree]} } with ..${parents: Seq[Tree]} { ${self: TmsValVarDef} => ..${stats: Seq[TmsStatement]} }" =>
        TmsTraitDefQQ(tree.pos, tree.symbol, mods, tName, tparams, unliftTmsValVarDefs(earlydefns), unliftTmsExpressions(parents), self, stats)
    }

    implicit val tmsTraitDefTreePreprocessor: TmsTreePreprocessor[TmsTraitDefQQ] = new TmsTreePreprocessor[TmsTraitDefQQ] {
      def preprocess(source: TmsTraitDefQQ): TmsTraitDefQQ = source.copy(
        earlyValDefs = source.earlyValDefs.preprocess,
        parents = source.parents.preprocess,
        // nothing to preprocess in selfValDef
        statements = source.statements.preprocess
      )
    }

    implicit val tmsTraitDefForExtractor: TmsForExtractor[TmsTraitDefQQ, Tree] = new TmsForExtractor[TmsTraitDefQQ, Tree] {
      def extract(tmsTraitDef: TmsTraitDefQQ, tmsForsBuilder: TmsForsBuilder)(implicit context: TmsExtractionContext): TmsForExtract[Tree] = {
        import tmsTraitDef._
        val TmsForExtract(extractedEarlyValDefsForsBuilder, extractedEarlyValDefs) = earlyValDefs.extract(tmsForsBuilder)
        val TmsForExtract(extractedParentsForsBuilder, extractedParents) = parents.extract(extractedEarlyValDefsForsBuilder)
        val TmsForExtract(extractedSelfForsBuilder, extractedSelf) = selfValDef.extract(extractedParentsForsBuilder)
        val TmsForExtract(extractedTraitDefForsBuilder, extractedStatements) = statements.extract(extractedSelfForsBuilder)
        val liftedTree = q"$modifiers trait $typeName[..$typeParameters] extends { ..$extractedEarlyValDefs } with ..$extractedParents { $extractedSelf => ..$extractedStatements }"
        c.internal.setSymbol(liftedTree, symbol)
        TmsForExtract(extractedTraitDefForsBuilder, liftedTree)
      }
    }
  }

  // see class TmsObjectDefQQ comments
//  object TmsClassDefQQ {
//
//    implicit val tmsClassDefUnliftable: Unliftable[TmsClassDefQQ] = Unliftable[TmsClassDefQQ] {
//      case tree@q"${mods: Modifiers} class ${tName: TypeName}[..${tparams: Seq[TypeDef]}] ${ctorMods: Modifiers}(...${paramss: Seq[Seq[Tree]]}) extends { ..${earlydefns: Seq[Tree]} } with ..${parents: Seq[Tree]} { ${self: TmsValVarDef} => ..${stats: Seq[TmsStatement]} }" =>
//        TmsClassDefQQ(tree.pos, tree.symbol, mods, tName, tparams, ctorMods, unliftTmsValVarDefs(paramss), unliftTmsValVarDefs(earlydefns), unliftTmsExpressions(parents), self, stats)
//    }
//
//    implicit val tmsClassDefTreePreprocessor: TmsTreePreprocessor[TmsClassDefQQ] = new TmsTreePreprocessor[TmsClassDefQQ] {
//      def preprocess(source: TmsClassDefQQ): TmsClassDefQQ = source.copy(
//        parameters = source.parameters.preprocess,
//        earlyValDefs = source.earlyValDefs.preprocess,
//        parents = source.parents.preprocess,
//        // nothing to preprocess in selfValDef
//        statements = source.statements.preprocess
//      )
//    }
//
//    implicit val tmsClassDefForExtractor: TmsForExtractor[TmsClassDefQQ, Tree] = new TmsForExtractor[TmsClassDefQQ, Tree] {
//      def extract(tmsClassDef: TmsClassDefQQ, tmsForsBuilder: TmsForsBuilder): TmsForExtract[Tree] = {
//        import tmsClassDef._
//        val TmsForExtract(extractedParametersForsBuilder, extractedParameters) = parameters.extract(tmsForsBuilder)
//        val TmsForExtract(extractedEarlyValDefsForsBuilder, extractedEarlyValDefs) = earlyValDefs.extract(extractedParametersForsBuilder)
//        val TmsForExtract(extractedParentsForsBuilder, extractedParents) = parents.extract(extractedEarlyValDefsForsBuilder)
//        val TmsForExtract(extractedSelfForsBuilder, extractedSelf) = selfValDef.extract(extractedParentsForsBuilder)
//        val TmsForExtract(extractedClassDefForsBuilder, extractedStatements) = statements.extract(extractedSelfForsBuilder)
//        val liftedTree = q"$modifiers class $typeName[..$typeParameters] $ctorModifiers(...$extractedParameters) extends { ..$extractedEarlyValDefs } with ..$extractedParents { $extractedSelf => ..$extractedStatements }"
//        c.internal.setSymbol(liftedTree, symbol)
//        TmsForExtract(extractedClassDefForsBuilder, liftedTree)
//      }
//    }
//  }

  // see class TmsObjectDefQQ comments
//  object TmsObjectDefQQ {
//
//    implicit val tmsObjectDefUnliftable: Unliftable[TmsObjectDefQQ] = Unliftable[TmsObjectDefQQ] {
//      case tree@q"${mods: Modifiers} object ${tName: TermName} extends { ..${earlydefns: Seq[Tree]} } with ..${parents: Seq[Tree]} { ${self: TmsValVarDef} => ..${stats: Seq[TmsStatement]} }" =>
//        TmsObjectDefQQ(tree.pos, tree.symbol, mods, tName, unliftTmsValVarDefs(earlydefns), unliftTmsExpressions(parents), self, stats)
//    }
//
//    implicit val tmsObjectDefTreePreprocessor: TmsTreePreprocessor[TmsObjectDefQQ] = new TmsTreePreprocessor[TmsObjectDefQQ] {
//      def preprocess(source: TmsObjectDefQQ): TmsObjectDefQQ = source.copy(
//        earlyValDefs = source.earlyValDefs.preprocess,
//        parents = source.parents.preprocess,
//        // nothing to preprocess in selfValDef
//        statements = source.statements.preprocess
//      )
//    }
//
//    implicit val tmsObjectDefForExtractor: TmsForExtractor[TmsObjectDefQQ, Tree] = new TmsForExtractor[TmsObjectDefQQ, Tree] {
//      def extract(tmsObjectDef: TmsObjectDefQQ, tmsForsBuilder: TmsForsBuilder): TmsForExtract[Tree] = {
//        import tmsObjectDef._
//        val TmsForExtract(extractedEarlyValDefsForsBuilder, extractedEarlyValDefs) = earlyValDefs.extract(tmsForsBuilder)
//        val TmsForExtract(extractedParentsForsBuilder, extractedParents) = parents.extract(extractedEarlyValDefsForsBuilder)
//        val TmsForExtract(extractedSelfForsBuilder, extractedSelf) = selfValDef.extract(extractedParentsForsBuilder)
//        val TmsForExtract(extractedObjectDefForsBuilder, extractedStatements) = statements.extract(extractedSelfForsBuilder)
//        val liftedTree = q"$modifiers object $termName extends { ..$extractedEarlyValDefs } with ..$extractedParents { $extractedSelf => ..$extractedStatements }"
//        c.internal.setSymbol(liftedTree, symbol)
//        TmsForExtract(extractedObjectDefForsBuilder, liftedTree)
//      }
//    }
//  }

}
