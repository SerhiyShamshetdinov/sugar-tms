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
 * at 19.01.2021 12:39
 */

import scala.annotation.{StaticAnnotation, compileTimeOnly}
import scala.language.experimental.macros
import scala.reflect.macros.whitebox

/** replaceByTtsImplicits macro annotation replaces target abstract method definition by transparent types implicits
 * for types stack depth of 5, like in the following example:
 * {{{
 * @replaceByTtsImplicits @compileTimeOnly(ctom) def wrapString[T <: String](s: T): WrappedString
 * }}}
 * results:
 * {{{
 * [info] ...\src\main\scala\sands\monads\sugar\TmsImplicits.scala:223:4: replaceByTtsImplicits macros output:
 * [info] @compileTimeOnly(ctom) implicit def tts1wrapString[T1[_] <: AnyRef, T <: String](s: T1[T]): WrappedString = ???
 * [info] @compileTimeOnly(ctom) implicit def tts2wrapString[T1[_] <: AnyRef, T2[_] <: AnyRef, T <: String](s: T1[T2[T]]): WrappedString = ???
 * [info] @compileTimeOnly(ctom) implicit def tts3wrapString[T1[_] <: AnyRef, T2[_] <: AnyRef, T3[_] <: AnyRef, T <: String](s: T1[T2[T3[T]]]): WrappedString = ???
 * [info] @compileTimeOnly(ctom) implicit def tts4wrapString[T1[_] <: AnyRef, T2[_] <: AnyRef, T3[_] <: AnyRef, T4[_] <: AnyRef, T <: String](s: T1[T2[T3[T4[T]]]]): WrappedString = ???
 * [info] @compileTimeOnly(ctom) implicit def tts5wrapString[T1[_] <: AnyRef, T2[_] <: AnyRef, T3[_] <: AnyRef, T4[_] <: AnyRef, T5[_] <: AnyRef, T <: String](s: T1[T2[T3[T4[T5[T]]]]]): WrappedString = ???
 * [info]   @replaceByTtsImplicits @compileTimeOnly(ctom) def wrapString[T <: String](s: T): WrappedString
 * [info]    ^
 * }}}
 */
@compileTimeOnly("enable macro paradise to expand macro annotations")
class replaceByTtsImplicits extends StaticAnnotation {
  def macroTransform(annottees: Any*): Any = macro TmsMacroAnnotations.replaceByTtsImplicitsImpl
}

/** replaceByPublicInterfacesDiff macro annotation fills target trait with diff of public interfaces (recursively with parents, skipping defined types) of 2 specified types.
 * It currently is used as helper for building the list of required tests and normally is commented.
 *
 * Found limitation are:
 *
 * - if TakeMembersOfType has Object method override, for example, clone(t), then error occurs (we should extend AnyRef or Object?)
 *
 * Usage sample:
 * {{{
 * @replaceByPublicInterfacesDiff @compileTimeOnly(ctom) trait DebugWrappedArrayPartialInterface[AOT] {
 *   // for parametrized trait the @uncheckedVariance ignores lots of variants errors (I see non actual)
 *   // while applying type parameter value of TakeMembersOfType to it's methods of covariant typed parents, like:
 *   // [error] covariant type AOT occurs in invariant position in type => scala.collection.parallel.mutable.ParArray[AOT] of method par
 *   type TakeMembersOfType = mutable.WrappedArray[AOT/*@uncheckedVariance*/]
 *   type DropMembersOfType = mutable.ArrayOps[AOT] @@ Object
 *
 *   <preserved part of the body>
 * }
 * }}}
 *
 * `@@ type may be used to conjunct the set of public methods of 2 types like in example above (for both TakeMembersOfType & DropMembersOfType). It is useful for final classes
 */
@compileTimeOnly("enable macro paradise to expand macro annotations")
class replaceByPublicInterfacesDiff extends StaticAnnotation {
  def macroTransform(annottees: Any*): Any = macro TmsMacroAnnotations.replaceByPublicInterfacesDiffImpl
}

class TmsMacroAnnotations(val c: whitebox.Context) {
  import c.universe._

  private val MaxTtsNumber = 5.ensuring(_ > 0)

  def replaceByTtsImplicitsImpl(annottees: c.Expr[Any]*): c.Expr[Any] = {

    def abortMacro = c.abort(c.enclosingPosition, "replaceByTtsImplicits macro annotation supports only abstract method definition without modifiers having exactly 1 type and exactly 1 parameter")

    val inputs = annottees.map(_.tree)
    val outputs = inputs match {
      case q"@..$annotations def ${defTerm: TermName}[..$typeParameters]($parameterTerm: ${parameterType: Tree}): $defType" :: Nil =>
        val typeParameter = typeParameters match {
          case Seq(tp) => tp
          case _ => abortMacro
        }

        def cloneForStackDepth(depth: Int): Tree = {
          val overloadTerm = TermName(s"tts$depth${defTerm.toString}")
          val stackTypes = (1 to depth).toList.map { level =>
            // val typeName = TypeName("T" + level)
            // tq"$typeName[_]"
            //[error] java.lang.IllegalArgumentException: can't splice T1[_$1] forSome {
            //[error]   <synthetic> type _$1
            //[error] } as type parameter
            // such built List of types is not then acceptable as type parameters in further q"def name[..$LIST, TT]": 2.11? due to absence of Modifiers(Flag.PARAM) ?
            TypeDef(Modifiers(Flag.PARAM), TypeName("T" + level),
              List(TypeDef(Modifiers(Flag.PARAM), typeNames.WILDCARD, List(), TypeBoundsTree(EmptyTree, EmptyTree))),
              TypeBoundsTree(EmptyTree, tq"AnyRef") // Tn <: AnyRef is required to make priority order of tts1xxx-tts5xxx groups. tts5xxx has the highest priority
            )
          }

          def stackType(level: Int, tpe: Tree): Tree =
            if (level > depth)
              tpe
            else {
              val typeName = TypeName("T" + level)
              tq"$typeName[${stackType(level + 1, tpe)}]"
            }

          q"@..$annotations implicit def $overloadTerm[..$stackTypes, $typeParameter]($parameterTerm: ${stackType(1, parameterType)}): $defType = ???;"
        }

        (1 to MaxTtsNumber).toList.map(cloneForStackDepth(_))

      case _ => abortMacro
    }
//    c.info(c.enclosingPosition, s"replaceByTtsImplicits macros input:\n${annottees.head.tree}", force = true)
//    c.info(c.enclosingPosition, s"replaceByTtsImplicits macros output:\n${outputs.map(showCode(_)).mkString("\n")}", force = true)
    c.Expr[Any](Block(outputs, Literal(Constant(()))))
  }

  def typeOfTheTree(typeTree: Tree, defns: List[Tree]): Type = c.typecheck(tq"$typeTree forSome { ..$defns }", c.TYPEmode).tpe.dealias

  def getPublicMethodMembers(tpe: Type): List[MethodSymbol] =
    tpe.members.sorted.collect {
      case s if s.isPublic && !s.isConstructor && s.isMethod => s.asMethod
    } //.map(s => internal.resetFlag(s, internal.flags(s))) // resets all flags. Uncomment for internal.valDef & internal.defDef commented usages

  def liftMethodSymbolSignature(s: MethodSymbol): Tree = s match {
    case valSymbol if valSymbol.isStable =>
      //            internal.valDef(internal.setFlag(valSymbol, Flag.DEFERRED)) - looks like it works here
      ValDef(
        Modifiers(Flag.DEFERRED),
        valSymbol.name,
        tq"${valSymbol.returnType}",
        EmptyTree
      )
    case defSymbol =>
      //            internal.defDef(internal.setFlag(defSymbol, Flag.DEFERRED/* | Flag.ABSTRACT*/), EmptyTree) - does not work here correctly?: 1) not found type scala/Any when no diff(Object) ??? 2) method is not reachable in trait (should change the owner ?)
      DefDef(
        Modifiers(Flag.DEFERRED),
        defSymbol.name,
        defSymbol.typeParams.map(internal.typeDef(_)),
        defSymbol.paramLists.map(_.map(internal.valDef(_))),
        tq"${defSymbol.returnType}",
        EmptyTree
      )
  }

  def replaceByPublicInterfacesDiffImpl(annottees: c.Expr[Any]*): c.Expr[Any] = {
    val inputs = annottees.map(_.tree)

    def abortMacro = c.abort(c.enclosingPosition, s"replaceByPublicInterfacesDiff supports trait only definition with 2 defined types TakeMembersOfType & DropMembersOfType + free body copied to output. Invalid input:\n$inputs")

    val output = inputs match {
      case q"$mods trait ${traitTypeName: TypeName}[..${typeParams: List[Tree]}] extends { ..$earlydefns } with ..$parents { $self => type TakeMembersOfType = ${takeMembersTypeTree: Tree}; type DropMembersOfType = ${dropMembersTypeTree: Tree}; ..$statements }" :: Nil =>

        val takeMembersType = typeOfTheTree(takeMembersTypeTree, typeParams)
        val dropMembersType = typeOfTheTree(dropMembersTypeTree, typeParams)

        val takeMethodInfos = getPublicMethodMembers(takeMembersType).map(MethodInfo(takeMembersType, _))
        val dropMethodInfos = getPublicMethodMembers(dropMembersType).map(MethodInfo(dropMembersType, _))
        MethodInfo.info(s"Public members of input TakeMembersOfType = $takeMembersType", takeMethodInfos)
        MethodInfo.info(s"Public members of input DropMembersOfType = $dropMembersType", dropMethodInfos)

        val droppedByRefMethodInfos = takeMethodInfos.filter(tmi => dropMethodInfos.exists(_.ms == tmi.ms))

        def partitionMethodInfos(sourceInfos: List[MethodInfo], dropInfos: List[MethodInfo]): (List[MethodInfo], List[MethodInfo]) =
          sourceInfos.partition(mi => dropInfos.exists(_.conflictsWith(mi)))

        val (conflictingMethodInfos, uniqueMethodInfos) = partitionMethodInfos(takeMethodInfos.diff(droppedByRefMethodInfos), dropMethodInfos.diff(droppedByRefMethodInfos))

        val droppedMethodInfos = droppedByRefMethodInfos ++ conflictingMethodInfos
        MethodInfo.info(s"Dropped members of $takeMembersType present in $dropMembersType", droppedMethodInfos)
        MethodInfo.info(s"Unique members of $takeMembersType absent in $dropMembersType", uniqueMethodInfos)

        val similarMethodInfos = dropMethodInfos.filter(dmi => uniqueMethodInfos.exists(umi => umi.name == dmi.name && umi.ms.paramLists.map(_.size) == dmi.ms.paramLists.map(_.size)))
        if (similarMethodInfos.nonEmpty) {
          MethodInfo.info(s"Non dropped symbols defined in $dropMembersType similar to diff one", similarMethodInfos)
        }

        val liftedMethodSymbols = uniqueMethodInfos.map(_.clonedMethodSymbol).map(liftMethodSymbolSignature(_))

        q"$mods trait $traitTypeName[..$typeParams] extends { ..$earlydefns } with ..$parents { $self => ..$statements; ..$liftedMethodSymbols }"

      case _ => abortMacro
    }
    c.info(c.enclosingPosition, s"replaceByPublicInterfacesDiff macros output:\n${/*showCode(*/output/*)*/}", force = true)

    c.Expr[Any](output)
  }

  private case class MethodInfo(ms: MethodSymbol, typeSignatureInSource: Type) {
    def name: String = ms.name.toString

    lazy val erasureTypeSignatureWithoutCallByName: String = {
      //println(s"$ms paramLists: ${ms.typeSignature.paramLists}, typeParams: ${ms.typeSignature.typeParams} ")
      typeSignatureInSource.erasure.toString.replaceFirst("^=> ", "()").replaceFirst("]=> ", "]()")
    }

    def conflictsWith(ms: MethodInfo): Boolean =
      name == ms.name &&
        (erasureTypeSignatureWithoutCallByName == ms.erasureTypeSignatureWithoutCallByName ||
          // maybe without erasure ?
          typeSignatureInSource.erasure.typeConstructor.finalResultType =:= ms.typeSignatureInSource.erasure.typeConstructor.finalResultType && {
            val pl = typeSignatureInSource.erasure.typeConstructor.paramLists.flatten.map(_.typeSignature)
            val plms = ms.typeSignatureInSource.erasure.typeConstructor.paramLists.flatten.map(_.typeSignature)
            pl.length == plms.length && pl.zip(plms).forall(tp => tp._1 =:= tp._2)
          }
        )

    def clonedMethodSymbol: MethodSymbol = internal.setInfo(internal.newMethodSymbol(NoSymbol, ms.name), typeSignatureInSource)
  }

  private object MethodInfo {
    def apply(sourceType: Type, ms: MethodSymbol): MethodInfo = new MethodInfo(ms, ms.infoIn(sourceType))

    def info(preface: String, methodInfos: List[MethodInfo]): Unit = { // with flags ${internal.flags(s).asInstanceOf[Long].toHexString}
      val infos = methodInfos.map(mi => s"${mi.name} ${mi.typeSignatureInSource} with source signature ${mi.ms} ${mi.ms.info} /erasure ${mi.erasureTypeSignatureWithoutCallByName}/ of ${mi.ms.owner}, isSynthetic=${mi.ms.isSynthetic}")
      c.info(c.enclosingPosition, s"$preface:\n${infos.mkString("\n")}", force = false)
    }
  }

}
