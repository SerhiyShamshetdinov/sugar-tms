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

package sands.sugar.tms.predef

import sands.sugar.tms.TmsForTestOptions._
import sands.sugar.tms.{SemanticVersion, TmsTestBase}

import scala.Seq
import scala.reflect.ClassTag
import scala.reflect.runtime.universe.{TypeTag, typeTag}

/*
 * Created by Serhiy Shamshetdinov
 * at 13.04.2021 14:28
 */

trait TmsForArrayTests extends TmsTestBase {

  def arrayTestsOf[T: TypeTag](tmsOptions: Seq[String], testArray: String, arrayInstance: Array[T], firstLem: String, firstInstance: T): Unit = {
    val options = literalOptions(tmsOptions)

    val classIsAnyVal: Boolean = typeTag[T].tpe <:< typeTag[AnyVal].tpe //classTag[T] <:< classTag[AnyVal] // classSimpleName != classSimpleName.capitalize
    val lemTypeName: String = typeTag[T].tpe.typeSymbol.name.toString
    //    typeTag[AnyVal].tpe.typeSymbol.name.toString  res0: String = AnyVal
    //    typeTag[AnyRef].tpe.typeSymbol.name.toString  res1: String = Object
    //    typeTag[Any].tpe.typeSymbol.name.toString     res2: String = Any
    //    typeTag[Int].tpe.typeSymbol.name.toString     res4: String = Int
    implicit val arrayInstanceElemTag: ClassTag[T] = arrayInstance.elemTag

    behavior of s"tms macros on Array[$lemTypeName] implicits with TmsOptions" + options

    // --- Native Array[T] methods

    //    @replaceByPublicInterfacesDiff @compileTimeOnly(ctom) trait DebugArrayCharPartialInterface {
    //      type TakeMembersOfType = Array[Char]
    //      type DropMembersOfType = Object
    //    }

    implicitExtensionOnTtsStacksTests(options, testValue = testArray, testValueType = s"Array[$lemTypeName]", typecheckedImplicitMethod = "", implicitType = "", (Seq(
      // StackTestData(accessor: String, aggregator: String, result: Any, resultType: String, implicitConversionShouldExist: Boolean = true)
      StackTestData("(0).toString", "+", arrayInstance(0).toString * MaxTtsNumber, "String"),
      StackTestData(".apply(0).toString", "+", arrayInstance(0).toString * MaxTtsNumber, "String"),
      StackTestData(s".update(0, $firstLem).toString", "+", "()" * MaxTtsNumber, "String")
      //Try StackTestData(".clone().mkString", "+", arrayInstance.mkString * MaxTtsNumber, "String"),
    ) ++
      (if (lemTypeName != "Char") Seq( // for Array[Char] see sands.sugar.tms.predef.TmsForCharArrayTest
        StackTestData(".length", "+", arrayInstance.length * MaxTtsNumber, "Int")
      ) else Nil)
      ): _*
    )

    // --- Array[T] extension by implicit conversion to mutable.ArrayOps[T] via *arrayOps implicits

    //    @replaceByPublicInterfacesDiff @compileTimeOnly(ctom) trait DebugArrayOpsPartialInterface[AOT] {
    //      // for parametrized trait the @uncheckedVariance ignores lots of variants errors (I see non actual) while
    //      // applying type parameter value of TakeMembersOfType to it's methods of covariant typed parents, like:
    //      // [error] covariant type AOT occurs in invariant position in type => scala.collection.parallel.mutable.ParArray[AOT] of method par
    //      type TakeMembersOfType = mutable.ArrayOps[AOT@uncheckedVariance]
    //      type DropMembersOfType = Array[AOT]
    //    }

    val implicitArrayOpsMethodName =
      if (lemTypeName == "Unit") "unitArrayOps"
      else if (lemTypeName == "Any" || lemTypeName == "AnyVal") "genericArrayOps"
      else if (classIsAnyVal) lemTypeName.toLowerCase + "ArrayOps"
      else "refArrayOps"

    val ArrayOpsStackTests = Seq(
      // StackTestData(accessor: String, aggregator: String, result: Any, resultType: String, implicitConversionShouldExist: Boolean = true)
      // ArrayOps
      StackTestData(".copyToArray(new Array[Any](1), 1, 1).toString", "+", "()" * MaxTtsNumber, "String"),
      StackTestData(".toArray[Any].length", "+", arrayInstance.length * MaxTtsNumber, "Int"),
      StackTestData(s".:+($firstLem).mkString", "+", arrayInstance.:+(firstInstance).mkString * MaxTtsNumber, "String"),
      StackTestData(s".:+($testArray).length", "+", arrayInstance.:+(arrayInstance).length * MaxTtsNumber, "Int"),
      StackTestData(s".+:($firstLem).mkString", "+", arrayInstance.+:(firstInstance).mkString * MaxTtsNumber, "String"),
      StackTestData(s".+:($testArray).length", "+", arrayInstance.+:(arrayInstance).length * MaxTtsNumber, "Int"),
      StackTestData(".par.mkString", "+", arrayInstance.mkString * MaxTtsNumber, "String"),
      //Try method flatten[U](implicit asTrav: T => Traversable[U], implicit m: scala.reflect.ClassTag[U])Array[U] with signature flatten[U](implicit asTrav: AOT @scala.annotation.unchecked.uncheckedVariance => Traversable[U], implicit m: scala.reflect.ClassTag[U])Array[U] /erasure (asTrav: Function1, implicit m: scala.reflect.ClassTag)Object/ of trait ArrayOps, isSynthetic=false
      //Fails even in worksheet with java.lang.NullPointerException
      //  at java.lang.reflect.Array.newArray(Native Method)
      // StackTestData(s".transpose(l => Array[$lemTypeName](l))(0)(0).toString", "+", arrayInstance(0).toString * MaxTtsNumber, "String"),
      StackTestData(s".unzip(l => (l, l), classTag[$lemTypeName], classTag[$lemTypeName])._1.mkString", "+", arrayInstance.mkString * MaxTtsNumber, "String"),
      StackTestData(s".unzip3(l => (l, l, l), classTag[$lemTypeName], classTag[$lemTypeName], classTag[$lemTypeName])._1.mkString", "+", arrayInstance.mkString * MaxTtsNumber, "String"),
      StackTestData(".seq.mkString", "+", arrayInstance.mkString * MaxTtsNumber, "String"),
      // ArrayLike
      StackTestData(".deep.mkString", "+", arrayInstance.deep.mkString * MaxTtsNumber, "String"),
      //Try  foreach()
      StackTestData(s".forall(_ => false)", "|", false, "Boolean"),
      StackTestData(s".exists(_ => true)", "&", true, "Boolean"),
      StackTestData(s".find(_ => false)", "orElse", None, s"Option[$lemTypeName]"),
      StackTestData(s".foldLeft($firstLem)((b, t) => b).toString", "+", arrayInstance.foldLeft(firstInstance)((b, t) => b).toString * MaxTtsNumber, "String"),
      StackTestData(s".foldRight($firstLem)((b, t) => t).toString", "+", arrayInstance.foldRight(firstInstance)((b, t) => t).toString * MaxTtsNumber, "String"),
      StackTestData(s".reduceLeft((b, t) => b).toString", "+", arrayInstance.reduceLeft((b, t) => b).toString * MaxTtsNumber, "String"),
      StackTestData(s".reduceRight((b, t) => b).toString", "+", arrayInstance.reduceRight((b, t) => b).toString * MaxTtsNumber, "String"),
      StackTestData(s".zip($testArray).length", "+", arrayInstance.length * MaxTtsNumber, "Int"),
      StackTestData(".zipWithIndex.length", "+", arrayInstance.length * MaxTtsNumber, "Int"),
      StackTestData(".slice(0, 1).mkString", "+", arrayInstance.head.toString * MaxTtsNumber, "String"),
      StackTestData(".head.toString", "+", arrayInstance.head.toString * MaxTtsNumber, "String"),
      StackTestData(".tail.mkString", "+", arrayInstance.tail.mkString * MaxTtsNumber, "String"),
      StackTestData(".last.toString", "+", arrayInstance.last.toString * MaxTtsNumber, "String"),
      StackTestData(".init.mkString", "+", arrayInstance.init.mkString * MaxTtsNumber, "String"),
      StackTestData(".take(1).mkString", "+", arrayInstance.take(1).mkString * MaxTtsNumber, "String"),
      StackTestData(".drop(1).mkString", "+", arrayInstance.drop(1).mkString * MaxTtsNumber, "String"),
      StackTestData(".takeRight(1).mkString", "+", arrayInstance.takeRight(1).mkString * MaxTtsNumber, "String"),
      StackTestData(".dropRight(1).mkString", "+", arrayInstance.dropRight(1).mkString * MaxTtsNumber, "String"),
      StackTestData(".splitAt(1)._1.mkString", "+", arrayInstance.splitAt(1)._1.mkString * MaxTtsNumber, "String"),
      StackTestData(s".takeWhile(_ => true).mkString", "+", arrayInstance.mkString * MaxTtsNumber, "String"),
      StackTestData(s".dropWhile(_ => false).mkString", "+", arrayInstance.mkString * MaxTtsNumber, "String"),
      StackTestData(s".span(_ => true)._1.mkString", "+", arrayInstance.mkString * MaxTtsNumber, "String"),
      StackTestData(s".sameElements($testArray.toSeq)", "&", true, "Boolean"),
      StackTestData(s".lengthCompare(0)", "+", arrayInstance.lengthCompare(0) * MaxTtsNumber, "Int"),
      StackTestData(s".segmentLength(_ => true, 0)", "+", arrayInstance.length * MaxTtsNumber, "Int"),
      StackTestData(s".indexWhere(_ => true, 0)", "+", 0 * MaxTtsNumber, "Int"),
      StackTestData(s".lastIndexWhere(_ => false, 0)", "+", -1 * MaxTtsNumber, "Int"),
      StackTestData(".reverse.mkString", "+", arrayInstance.reverse.mkString * MaxTtsNumber, "String"),
      StackTestData(".reverseIterator.next.toString", "+", arrayInstance.last.toString * MaxTtsNumber, "String"),
      StackTestData(s".startsWith($testArray, 1)", "|", false, "Boolean"),
      StackTestData(s".endsWith($testArray)", "&", true, "Boolean"),
      // IndexedSeqLike
      StackTestData(".view.length", "+", arrayInstance.length * MaxTtsNumber, "Int"),
      StackTestData(".view(0, 1).length", "+", 1 * MaxTtsNumber, "Int"),
      //Try StackTestData(".hashCode()", "+", indexedSeqChar.hashCode() * MaxTtsNumber, "Int"),
      StackTestData(".iterator.next.toString", "+", arrayInstance.iterator.next().toString * MaxTtsNumber, "String"),
      StackTestData(".toBuffer.mkString", "+", arrayInstance.toBuffer.mkString * MaxTtsNumber, "String"),
      // SeqLike
      StackTestData(".size", "+", arrayInstance.length * MaxTtsNumber, "Int"),
      StackTestData(".permutations.length", "+", arrayInstance.permutations.length * MaxTtsNumber, "Int"),
      StackTestData(".combinations(2).length", "+", arrayInstance.combinations(2).length * MaxTtsNumber, "Int"),
      StackTestData(".reverseMap(identity(_)).mkString", "+", arrayInstance.reverseMap(identity(_)).mkString * MaxTtsNumber, "String"),
      StackTestData(s".indexOfSlice($testArray)", "+", 0 * MaxTtsNumber, "Int"),
      StackTestData(s".indexOfSlice($testArray, 1)", "+", -1 * MaxTtsNumber, "Int"),
      StackTestData(s".lastIndexOfSlice($testArray)", "+", 0 * MaxTtsNumber, "Int"),
      StackTestData(s".lastIndexOfSlice($testArray, 1)", "+", 0 * MaxTtsNumber, "Int"),
      StackTestData(s".containsSlice($testArray)", "&", true, "Boolean"),
      StackTestData(s".contains($firstLem)", "&", true, "Boolean"),
      StackTestData(s".union($testArray).mkString", "+", arrayInstance.mkString * 2 * MaxTtsNumber, "String"),
      StackTestData(s".diff($testArray).mkString", "+", "" * MaxTtsNumber, "String"),
      StackTestData(s".intersect($testArray).mkString", "+", arrayInstance.mkString * MaxTtsNumber, "String"),
      StackTestData(".distinct.mkString", "+", arrayInstance.mkString * MaxTtsNumber, "String"),
      StackTestData(s".patch(0, $testArray, ${testArray.length}).mkString", "+", arrayInstance.mkString * MaxTtsNumber, "String"),
      StackTestData(s".updated(0, $firstLem).mkString", "+", arrayInstance.updated(0, firstInstance).mkString * MaxTtsNumber, "String"),
      StackTestData(s".:+[$lemTypeName, Seq[$lemTypeName]]($firstLem).mkString", "+", arrayInstance.:+[T, Seq[T]](firstInstance).mkString * MaxTtsNumber, "String"),
      StackTestData(s".+:[$lemTypeName, Seq[$lemTypeName]]($firstLem).mkString", "+", arrayInstance.+:[T, Seq[T]](firstInstance).mkString * MaxTtsNumber, "String"),
      StackTestData(s".padTo(4, $firstLem).mkString", "+", arrayInstance.padTo(4, firstInstance).mkString * MaxTtsNumber, "String"),
      StackTestData(s".corresponds($testArray)((a, b) => true)", "&", true, "Boolean"),
      StackTestData(".sortWith((a, b) => true).mkString", "+", arrayInstance.sortWith((a, b) => true).mkString * MaxTtsNumber, "String"),
      StackTestData(".sortBy(_ => 0).mkString", "+", arrayInstance.sortBy(_ => 0).mkString * MaxTtsNumber, "String"),
      StackTestData(".sorted[Any](new Ordering[Any] {def compare(x: Any, y: Any) = 0}).mkString", "+", arrayInstance.sorted[Any](new Ordering[Any] {
        def compare(x: Any, y: Any) = 0
      }).mkString * MaxTtsNumber, "String"),
      StackTestData(".toSeq.mkString", "+", arrayInstance.toSeq.mkString * MaxTtsNumber, "String"),
      StackTestData(".indices.head", "+", 0 * MaxTtsNumber, "Int"),
      // GenSeqLike
      StackTestData(".isDefinedAt(-1)", "|", false, "Boolean"),
      StackTestData(".prefixLength(_ => true)", "+", arrayInstance.length * MaxTtsNumber, "Int"),
      StackTestData(".indexWhere(_ => true)", "+", 0 * MaxTtsNumber, "Int"),
      StackTestData(s".indexOf($firstLem)", "+", 0 * MaxTtsNumber, "Int"),
      StackTestData(s".indexOf($firstLem, 0)", "+", 0 * MaxTtsNumber, "Int"),
      StackTestData(s".lastIndexOf($firstLem)", "+", 0 * MaxTtsNumber, "Int"),
      StackTestData(s".lastIndexOf($firstLem, 0)", "+", 0 * MaxTtsNumber, "Int"),
      StackTestData(".lastIndexWhere(_ => false)", "+", -1 * MaxTtsNumber, "Int"),
      StackTestData(s".startsWith($testArray)", "&", true, "Boolean"),
      //Try method equals(that: Any)Boolean with signature equals(that: Any)Boolean /erasure (that: Object)Boolean/ of trait GenSeqLike, isSynthetic=false
      // IterableLike
      StackTestData(".toIterable.iterator.hasNext", "&", true, "Boolean"),
      StackTestData(".toIterator.hasNext", "&", true, "Boolean"),
      StackTestData(".grouped(1).hasNext", "&", true, "Boolean"),
      StackTestData(".sliding(1).hasNext", "&", true, "Boolean"),
      StackTestData(".sliding(1, 1).hasNext", "&", true, "Boolean"),
      StackTestData(s".zipAll($testArray, $firstLem, $firstLem).length", "+", arrayInstance.length * MaxTtsNumber, "Int"),
      StackTestData(".toStream.mkString", "+", arrayInstance.mkString * MaxTtsNumber, "String"),
      //Try 2.12 StackTestData(s".canEqual($testArray)", "&", true, "Boolean"),
      // TraversableLike
      StackTestData(".repr.mkString", "+", arrayInstance.mkString * MaxTtsNumber, "String"),
      StackTestData(".isTraversableAgain", "&", true, "Boolean"),
      StackTestData(".hasDefiniteSize", "&", true, "Boolean"),
      StackTestData(s".++($testArray).mkString", "+", arrayInstance.mkString * 2 * MaxTtsNumber, "String"),
      StackTestData(s".++:($testArray).mkString", "+", arrayInstance.mkString * 2 * MaxTtsNumber, "String"),
      StackTestData(s".++:(Iterator($firstLem)).head.toString", "+", firstInstance.toString * MaxTtsNumber, "String"),
      //Try method map[B, That](f: A => B)(implicit bf: scala.collection.generic.CanBuildFrom[Repr,B,That])That with signature map[B, That](f: AOT @scala.annotation.unchecked.uncheckedVariance => B)(implicit bf: scala.collection.generic.CanBuildFrom[Array[AOT @scala.annotation.unchecked.uncheckedVariance],B,That])That /erasure (f: Function1, implicit bf: scala.collection.generic.CanBuildFrom)Object/ of trait TraversableLike, isSynthetic=false
      //Try method flatMap[B, That](f: A => scala.collection.GenTraversableOnce[B])(implicit bf: scala.collection.generic.CanBuildFrom[Repr,B,That])That with signature flatMap[B, That](f: AOT @scala.annotation.unchecked.uncheckedVariance => scala.collection.GenTraversableOnce[B])(implicit bf: scala.collection.generic.CanBuildFrom[Array[AOT @scala.annotation.unchecked.uncheckedVariance],B,That])That /erasure (f: Function1, implicit bf: scala.collection.generic.CanBuildFrom)Object/ of trait TraversableLike, isSynthetic=false
      //Try method filter(p: A => Boolean)Repr with signature filter(p: AOT @scala.annotation.unchecked.uncheckedVariance => Boolean)Array[AOT @scala.annotation.unchecked.uncheckedVariance] /erasure (p: Function1)Array[Object]/ of trait TraversableLike, isSynthetic=false
      //Try method filterNot(p: A => Boolean)Repr with signature filterNot(p: AOT @scala.annotation.unchecked.uncheckedVariance => Boolean)Array[AOT @scala.annotation.unchecked.uncheckedVariance] /erasure (p: Function1)Array[Object]/ of trait TraversableLike, isSynthetic=false
      StackTestData(s".collect{case c => c}.mkString", "+", arrayInstance.mkString * MaxTtsNumber, "String"),
      StackTestData(".partition(_ => true)._1.mkString", "+", arrayInstance.mkString * MaxTtsNumber, "String"),
      StackTestData(".groupBy(_ => true)(true).mkString", "+", arrayInstance.mkString * MaxTtsNumber, "String"),
      StackTestData(s".scan($firstLem)((c1, c2) => c1).mkString", "+", arrayInstance.scan(firstInstance)((c1, c2) => c1).mkString * MaxTtsNumber, "String"),
      StackTestData(s".scanLeft($firstLem)((c1, c2) => c1).mkString", "+", arrayInstance.scanLeft(firstInstance)((c1, c2) => c1).mkString * MaxTtsNumber, "String"),
      StackTestData(s".scanRight($firstLem)((c1, c2) => c2).mkString", "+", arrayInstance.scanRight(firstInstance)((c1, c2) => c2).mkString * MaxTtsNumber, "String"),
      StackTestData(".headOption", "orElse", arrayInstance.headOption, s"Option[$lemTypeName]"),
      StackTestData(".lastOption", "orElse", arrayInstance.lastOption, s"Option[$lemTypeName]"),
      StackTestData(".tails.next.mkString", "+", arrayInstance.mkString * MaxTtsNumber, "String"),
      StackTestData(".inits.next.mkString", "+", arrayInstance.mkString * MaxTtsNumber, "String"),
      StackTestData(".toTraversable.mkString", "+", arrayInstance.mkString * MaxTtsNumber, "String"),
      //Rich*.to(n) StackTestData(".to[Seq].mkString", "+", arrayInstance.mkString * MaxTtsNumber, "String"),
      StackTestData(".stringPrefix", "+", arrayInstance.stringPrefix * MaxTtsNumber, "String"),
      //Try method withFilter(p: A => Boolean)scala.collection.generic.FilterMonadic[A,Repr] with signature withFilter(p: AOT @scala.annotation.unchecked.uncheckedVariance => Boolean)scala.collection.generic.FilterMonadic[AOT @scala.annotation.unchecked.uncheckedVariance,Array[AOT @scala.annotation.unchecked.uncheckedVariance]] /erasure (p: Function1)scala.collection.generic.FilterMonadic/ of trait TraversableLike, isSynthetic=false
      // TraversableOnce
      StackTestData(".nonEmpty", "&", true, "Boolean"),
      StackTestData(".count(_ => false)", "+", 0 * MaxTtsNumber, "Int"),
      StackTestData(s".collectFirst{case c => c}.nonEmpty", "&", true, "Boolean"),
      StackTestData(s"./:($firstLem)((c1, c2) => c1).toString", "+", firstInstance.toString * MaxTtsNumber, "String"),
      StackTestData(s".:\\($firstLem)((c1, c2) => c2).toString", "+", firstInstance.toString * MaxTtsNumber, "String"),
      StackTestData(".reduceLeftOption((c1, c2) => c1).nonEmpty", "&", true, "Boolean"),
      StackTestData(".reduceRightOption((c1, c2) => c2).nonEmpty", "&", true, "Boolean"),
      StackTestData(".reduce((c1, c2) => c1).toString", "+", arrayInstance.head.toString * MaxTtsNumber, "String"),
      StackTestData(".reduceOption((c1, c2) => c1).nonEmpty", "&", true, "Boolean"),
      //Try 2.12 StackTestData(s".fold($firstLem)((c1, c2) => c1).toString", "+", firstInstance.toString * MaxTtsNumber, "String"),
      StackTestData(s".aggregate($firstLem)((c1, c2) => c1, (c1, c2) => c1).toString", "+", arrayInstance.aggregate(firstInstance)((c1, c2) => c1, (c1, c2) => c1).toString * MaxTtsNumber, "String"),
      //Numeric[T] StackTestData(".sum.toString", "+", arrayInstance.sum.toString * MaxTtsNumber, "String"),
      //Numeric[T] StackTestData(".product.toString", "+", arrayInstance.product.toString * MaxTtsNumber, "String"),
      //Rich*.min(n) StackTestData(".min(new Ordering[Any] {def compare(x: Any, y: Any) = 0}).toString", "+", arrayInstance.min(new Ordering[Any] {
      //  def compare(x: Any, y: Any) = 0
      //}).toString * MaxTtsNumber, "String"),
      //Rich*.max(n) StackTestData(".max(new Ordering[Any] {def compare(x: Any, y: Any) = 0}).toString", "+", arrayInstance.max(new Ordering[Any] {
      //  def compare(x: Any, y: Any) = 0
      //}).toString * MaxTtsNumber, "String"),
      StackTestData(".maxBy(_.toString).toString", "+", arrayInstance.maxBy(_.toString).toString * MaxTtsNumber, "String"),
      StackTestData(".minBy(_.toString).toString", "+", arrayInstance.minBy(_.toString).toString * MaxTtsNumber, "String"),
      StackTestData(s".copyToBuffer(scala.collection.mutable.Buffer[$lemTypeName]()).toString", "+", "()" * MaxTtsNumber, "String"),
      StackTestData(s".copyToArray(new Array[$lemTypeName]($testArray.length), 1).toString", "+", "()" * MaxTtsNumber, "String"),
      StackTestData(s".copyToArray(new Array[$lemTypeName]($testArray.length)).toString", "+", "()" * MaxTtsNumber, "String"),
      StackTestData(".toList.mkString", "+", arrayInstance.mkString * MaxTtsNumber, "String"),
      StackTestData(".toIndexedSeq.mkString", "+", arrayInstance.mkString * MaxTtsNumber, "String"),
      StackTestData(".toSet", "++", arrayInstance.toSet, s"Set[$lemTypeName]"),
      StackTestData(".toVector.mkString", "+", arrayInstance.mkString * MaxTtsNumber, "String"),
      //Tuple2 method toMap[T, U](implicit ev: <:<[A,(T, U)])scala.collection.immutable.Map[T,U] with signature toMap[T, U](implicit ev: <:<[Any @scala.annotation.unchecked.uncheckedVariance,(T, U)])scala.collection.immutable.Map[T,U] /erasure (ev: <:<)scala.collection.immutable.Map/ of trait TraversableOnce, isSynthetic=false
      StackTestData(""".mkString("<", "-", ">")""", "+", arrayInstance.mkString("<", "-", ">") * MaxTtsNumber, "String"),
      StackTestData(""".mkString("-")""", "+", arrayInstance.mkString("-") * MaxTtsNumber, "String"),
      StackTestData(".mkString", "+", arrayInstance.mkString * MaxTtsNumber, "String"),
      StackTestData(""".addString(new StringBuilder, "<", "-", ">").toString""", "+", arrayInstance.mkString("<", "-", ">") * MaxTtsNumber, "String"),
      StackTestData(""".addString(new StringBuilder, "-").toString""", "+", arrayInstance.mkString("-") * MaxTtsNumber, "String"),
      StackTestData(""".addString(new StringBuilder).toString""", "+", arrayInstance.mkString * MaxTtsNumber, "String")
    )

    val ArrayOpsIsEmptyStackTest = if (lemTypeName != "Char" // for any non-Char Array
      || (JavaStringVersion.startsWith("9.") || JavaStringVersion < "15") // for any JDK before 15 of Char Array
      || SemanticVersion(ScalaStringVersion) >= "2.12.13") { // for any JDK starting 15 & Scala starting 2.12.13 of Char Array
      // other version combinations: .isEmpty on Array[Char] is not compiled in general (when JDK >= 15 & Scala <= 2.12.12)
      // IndexedSeqOptimized
      Seq(StackTestData(".isEmpty", "|", false, "Boolean"))
    } else Seq()

    implicitExtensionOnTtsStacksTests(options, testValue = testArray, testValueType = s"Array[$lemTypeName]", typecheckedImplicitMethod = implicitArrayOpsMethodName, implicitType = s"ArrayOps[$lemTypeName]",
      ArrayOpsStackTests ++ ArrayOpsIsEmptyStackTest: _*
    )

    // --- Array[T] extension by implicit conversion to WrappedArray[T] via wrap*Array implicits

    //    @replaceByPublicInterfacesDiff @compileTimeOnly(ctom) trait DebugWrappedArrayPartialInterface[AOT] {
    //      // for parametrized trait the @uncheckedVariance ignores lots of variants errors (I see non actual)
    //      // while applying type parameter value of TakeMembersOfType to it's methods of covariant typed parents, like:
    //      // [error] covariant type AOT occurs in invariant position in type => scala.collection.parallel.mutable.ParArray[AOT] of method par
    //      type TakeMembersOfType = mutable.WrappedArray[AOT/*@uncheckedVariance*/]
    //      type DropMembersOfType = mutable.ArrayOps[AOT] @@ Object
    //    }

    val implicitWrapArrayMethodName =
      if (lemTypeName == "Unit") "wrapUnitArray"
      else if (lemTypeName == "Any" || lemTypeName == "AnyVal") "genericWrapArray"
      else if (classIsAnyVal) "wrap" + lemTypeName + "Array"
      else "wrapRefArray"

    implicitExtensionOnTtsStacksTests(options, testValue = testArray, testValueType = s"Array[$lemTypeName]", typecheckedImplicitMethod = implicitWrapArrayMethodName, implicitType = s"WrappedArray[$lemTypeName]",
      // StackTestData(accessor: String, aggregator: String, result: Any, resultType: String, implicitConversionShouldExist: Boolean = true)
      // WrappedArray
      StackTestData(".elemTag.newArray(1).length", "+", arrayInstance.elemTag.newArray(1).length * MaxTtsNumber, "Int"),
      StackTestData(".array.mkString", "+", arrayInstance.array.mkString * MaxTtsNumber, "String"),
      //Deprecated StackTestData(".elemManifest.newArray(1).length", "+", arrayInstance.elemManifest.newArray(1).length * MaxTtsNumber, "Int"),
      //Try method clone()scala.collection.mutable.WrappedArray[T] with signature clone()scala.collection.mutable.WrappedArray[AOT] /erasure ()scala.collection.mutable.WrappedArray/ of class WrappedArray, isSynthetic=false
      // IndexedSeq
      StackTestData(".companion.empty.mkString", "+", arrayInstance.companion.empty.mkString * MaxTtsNumber, "String"),
      //Try StackTestData(".transform(identity(_)).mkString", "+", arrayInstance.mkString * MaxTtsNumber, "String"),
      // PartialFunction
      //Try method orElse[A1 <: A, B1 >: B](that: PartialFunction[A1,B1])PartialFunction[A1,B1] with signature orElse[A1 <: Int, B1 >: Any](that: PartialFunction[A1,B1])PartialFunction[A1,B1] /erasure (that: PartialFunction)PartialFunction/ of trait PartialFunction, isSynthetic=false
      //Try method andThen[C](k: B => C)PartialFunction[A,C] with signature andThen[C](k: Any => C)PartialFunction[Int,C] /erasure (k: Function1)PartialFunction/ of trait PartialFunction, isSynthetic=false
      StackTestData(".lift(0)", "orElse", arrayInstance.headOption, s"Option[$lemTypeName]"),
      StackTestData(s".applyOrElse(0, (_: Int) => $firstInstance).toString", "+", arrayInstance.head.toString * MaxTtsNumber, "String"),
      StackTestData(".runWith(identity(_))(0)", "&", true, "Boolean"),
      // Function1
      StackTestData(".compose[Int](identity(_))(0).toString", "+", arrayInstance.head.toString * MaxTtsNumber, "String"),
      // GenericTraversableTemplate
      StackTestData(s".genericBuilder[$lemTypeName].result().mkString", "+", "" * MaxTtsNumber, "String")
      //Try method flatten[B](implicit asTraversable: A => scala.collection.GenTraversableOnce[B])CC[B] with signature flatten[B](implicit asTraversable: Any => scala.collection.GenTraversableOnce[B])scala.collection.mutable.IndexedSeq[B] /erasure (asTraversable: Function1)scala.collection.mutable.IndexedSeq/ of trait GenericTraversableTemplate, isSynthetic=false
    )
  }
}
