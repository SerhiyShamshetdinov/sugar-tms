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

import sands.sugar.tms.TmsTestBase

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
    implicit val arrayInstanceElemTag: ClassTag[T] = arrayInstance.elemTag.asInstanceOf[ClassTag[T]] //.asInstanceOf[ClassTag[T]] for 2.13 - it is now `ClassTag[_]`

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

    // --- Array[T] extension by implicit conversion to ArrayOps[T] via *arrayOps implicits

//    import sands.sugar.tms.TmsImplicitsCommon.ctom
//    import sands.sugar.tms.replaceByPublicInterfacesDiff
//
//    import scala.annotation.compileTimeOnly
//    import scala.annotation.unchecked.uncheckedVariance
//    import scala.collection.ArrayOps
//    @replaceByPublicInterfacesDiff @compileTimeOnly(ctom) trait DebugArrayOpsPartialInterface[AOT] {
//      // for parametrized trait the @uncheckedVariance ignores lots of variants errors (I see non actual) while
//      // applying type parameter value of TakeMembersOfType to it's methods of covariant typed parents, like:
//      // [error] covariant type AOT occurs in invariant position in type => scala.collection.parallel.mutable.ParArray[AOT] of method par
//      type TakeMembersOfType = ArrayOps[AOT/*@uncheckedVariance*/]
//      type DropMembersOfType = Array[AOT]
//    }

    val implicitArrayOpsMethodName =
      if (lemTypeName == "Unit") "unitArrayOps"
      else if (lemTypeName == "Any" || lemTypeName == "AnyVal") "genericArrayOps"
      else if (classIsAnyVal) lemTypeName.toLowerCase + "ArrayOps"
      else "refArrayOps"

    implicitExtensionOnTtsStacksTests(options, testValue = testArray, testValueType = s"Array[$lemTypeName]", typecheckedImplicitMethod = implicitArrayOpsMethodName, implicitType = s"ArrayOps[$lemTypeName]",
      // StackTestData(accessor: String, aggregator: String, result: Any, resultType: String, implicitConversionShouldExist: Boolean = true)
      // ArrayOps
      StackTestData(".size", "+", arrayInstance.size * MaxTtsNumber, "Int"),
      StackTestData(".knownSize", "+", arrayInstance.knownSize * MaxTtsNumber, "Int"),
      StackTestData(".isEmpty", "|", false, "Boolean"),
      StackTestData(".nonEmpty", "&", true, "Boolean"),
      StackTestData(".head.toString", "+", arrayInstance.head.toString * MaxTtsNumber, "String"),
      StackTestData(".last.toString", "+", arrayInstance.last.toString * MaxTtsNumber, "String"),
      StackTestData(".headOption", "orElse", arrayInstance.headOption, s"Option[$lemTypeName]"),
      StackTestData(".lastOption", "orElse", arrayInstance.lastOption, s"Option[$lemTypeName]"),
      StackTestData(".sizeCompare(0)", "+", arrayInstance.sizeCompare(0) * MaxTtsNumber, "Int"),
      StackTestData(".lengthCompare(0)", "+", arrayInstance.lengthCompare(0) * MaxTtsNumber, "Int"),
      StackTestData(".sizeIs > 0", "&", true, "Boolean"),
      StackTestData(".lengthIs > 0", "&", true, "Boolean"),
      StackTestData(".slice(0, 1).mkString", "+", arrayInstance.head.toString * MaxTtsNumber, "String"),
      StackTestData(".tail.mkString", "+", arrayInstance.tail.mkString * MaxTtsNumber, "String"),
      StackTestData(".init.mkString", "+", arrayInstance.init.mkString * MaxTtsNumber, "String"),
      StackTestData(".tails.next.mkString", "+", arrayInstance.mkString * MaxTtsNumber, "String"),
      StackTestData(".inits.next.mkString", "+", arrayInstance.mkString * MaxTtsNumber, "String"),
      StackTestData(".take(1).mkString", "+", arrayInstance.take(1).mkString * MaxTtsNumber, "String"),
      StackTestData(".drop(1).mkString", "+", arrayInstance.drop(1).mkString * MaxTtsNumber, "String"),
      StackTestData(".takeRight(1).mkString", "+", arrayInstance.takeRight(1).mkString * MaxTtsNumber, "String"),
      StackTestData(".dropRight(1).mkString", "+", arrayInstance.dropRight(1).mkString * MaxTtsNumber, "String"),
      StackTestData(".takeWhile(_ => true).mkString", "+", arrayInstance.mkString * MaxTtsNumber, "String"),
      StackTestData(".dropWhile(_ => false).mkString", "+", arrayInstance.mkString * MaxTtsNumber, "String"),
      StackTestData(".iterator.next.toString", "+", arrayInstance.iterator.next().toString * MaxTtsNumber, "String"),
      StackTestData(".stepper.hasStep", "&", true, "Boolean"),
      StackTestData(".grouped(1).hasNext", "&", true, "Boolean"),
      StackTestData(".span(_ => true)._1.mkString", "+", arrayInstance.mkString * MaxTtsNumber, "String"),
      StackTestData(".splitAt(1)._1.mkString", "+", arrayInstance.splitAt(1)._1.mkString * MaxTtsNumber, "String"),
      StackTestData(".partition(_ => true)._1.mkString", "+", arrayInstance.mkString * MaxTtsNumber, "String"),
      StackTestData(".partitionMap(Left(_))._1.mkString", "+", arrayInstance.mkString * MaxTtsNumber, "String"),
      StackTestData(".reverse.mkString", "+", arrayInstance.reverse.mkString * MaxTtsNumber, "String"),
      StackTestData(".reverseIterator.next.toString", "+", arrayInstance.last.toString * MaxTtsNumber, "String"),
      //Try filter (p: AOT => Boolean): Array[AOT] with source signature method filter (p: A => Boolean): Array[A] /erasure (p: Function1): Object/ of class ArrayOps, isSynthetic=false
      StackTestData(".filterNot(_ => false).mkString", "+", arrayInstance.mkString * MaxTtsNumber, "String"),
      StackTestData(".sorted[Any](new Ordering[Any] {def compare(x: Any, y: Any) = 0}).mkString", "+", arrayInstance.sorted[Any](new Ordering[Any] {
        def compare(x: Any, y: Any) = 0
      }).mkString * MaxTtsNumber, "String"),
      StackTestData(".sortWith((a, b) => true).mkString", "+", arrayInstance.sortWith((a, b) => true).mkString * MaxTtsNumber, "String"),
      StackTestData(".sortBy(_ => 0).mkString", "+", arrayInstance.sortBy(_ => 0).mkString * MaxTtsNumber, "String"),
      //Try method withFilter(p: A => Boolean)scala.collection.generic.FilterMonadic[A,Repr] with signature withFilter(p: AOT @scala.annotation.unchecked.uncheckedVariance => Boolean)scala.collection.generic.FilterMonadic[AOT @scala.annotation.unchecked.uncheckedVariance,Array[AOT @scala.annotation.unchecked.uncheckedVariance]] /erasure (p: Function1)scala.collection.generic.FilterMonadic/ of trait TraversableLike, isSynthetic=false
      StackTestData(s".indexOf($firstLem)", "+", 0 * MaxTtsNumber, "Int"),
      StackTestData(s".indexOf($firstLem, 0)", "+", 0 * MaxTtsNumber, "Int"),
      StackTestData(".indexWhere(_ => true)", "+", 0 * MaxTtsNumber, "Int"),
      StackTestData(s".indexWhere(_ => true, 0)", "+", 0 * MaxTtsNumber, "Int"),
      StackTestData(s".lastIndexOf($firstLem)", "+", 0 * MaxTtsNumber, "Int"),
      StackTestData(s".lastIndexOf($firstLem, 0)", "+", 0 * MaxTtsNumber, "Int"),
      StackTestData(".lastIndexWhere(_ => false)", "+", -1 * MaxTtsNumber, "Int"),
      StackTestData(s".lastIndexWhere(_ => false, 0)", "+", -1 * MaxTtsNumber, "Int"),
      StackTestData(".find(_ => false)", "orElse", None, s"Option[$lemTypeName]"),
      StackTestData(s".exists(_ => true)", "&", true, "Boolean"),
      StackTestData(".forall(_ => false)", "|", false, "Boolean"),
      StackTestData(s".foldLeft($firstLem)((b, t) => b).toString", "+", arrayInstance.foldLeft(firstInstance)((b, t) => b).toString * MaxTtsNumber, "String"),
      StackTestData(s".scanLeft($firstLem)((c1, c2) => c1).mkString", "+", arrayInstance.scanLeft(firstInstance)((c1, c2) => c1).mkString * MaxTtsNumber, "String"),
      StackTestData(s".scan($firstLem)((c1, c2) => c1).mkString", "+", arrayInstance.scan(firstInstance)((c1, c2) => c1).mkString * MaxTtsNumber, "String"),
      StackTestData(s".scanRight($firstLem)((c1, c2) => c2).mkString", "+", arrayInstance.scanRight(firstInstance)((c1, c2) => c2).mkString * MaxTtsNumber, "String"),
      StackTestData(s".foldRight($firstLem)((b, t) => t).toString", "+", arrayInstance.foldRight(firstInstance)((b, t) => t).toString * MaxTtsNumber, "String"),
      //Try StackTestData(s".fold($firstLem)((c1, c2) => c1).toString", "+", firstInstance.toString * MaxTtsNumber, "String"),
      //Try map [B](f: AOT => B)(implicit ct: scala.reflect.ClassTag[B]): Array[B] with source signature method map [B](f: A => B)(implicit ct: scala.reflect.ClassTag[B]): Array[B] /erasure (f: Function1, ct: scala.reflect.ClassTag): Object/ of class ArrayOps, isSynthetic=false
      StackTestData(".mapInPlace(identity(_)).mkString", "+", arrayInstance.mkString * MaxTtsNumber, "String"),
      //Try flatMap [B](f: AOT => scala.collection.IterableOnce[B])(implicit evidence$8: scala.reflect.ClassTag[B]): Array[B] with source signature method flatMap [B](f: A => scala.collection.IterableOnce[B])(implicit evidence$8: scala.reflect.ClassTag[B]): Array[B] /erasure (f: Function1, evidence$8: scala.reflect.ClassTag): Object/ of class ArrayOps, isSynthetic=false
      //Try flatMap [BS, B](f: AOT => BS)(implicit asIterable: BS => Iterable[B], m: scala.reflect.ClassTag[B]): Array[B] with source signature method flatMap [BS, B](f: A => BS)(implicit asIterable: BS => Iterable[B], m: scala.reflect.ClassTag[B]): Array[B] /erasure (f: Function1, asIterable: Function1, m: scala.reflect.ClassTag): Object/ of class ArrayOps, isSynthetic=false
      //Try flatten [B](implicit asIterable: AOT => scala.collection.IterableOnce[B], m: scala.reflect.ClassTag[B]): Array[B] with source signature method flatten [B](implicit asIterable: A => scala.collection.IterableOnce[B], m: scala.reflect.ClassTag[B]): Array[B] /erasure (asIterable: Function1, m: scala.reflect.ClassTag): Object/ of class ArrayOps, isSynthetic=false
      StackTestData(".collect{case c => c}.mkString", "+", arrayInstance.mkString * MaxTtsNumber, "String", implicitConversionShouldExist = false), // looks like it is inlined
      StackTestData(".collectFirst{case c => c}.nonEmpty", "&", true, "Boolean"),
      StackTestData(s".zip($testArray).length", "+", arrayInstance.length * MaxTtsNumber, "Int"),
      StackTestData(s".lazyZip($testArray).forall((_: (Any, Any)) => true)", "&", true, "Boolean"),
      StackTestData(s".zipAll($testArray, $firstLem, $firstLem).length", "+", arrayInstance.length * MaxTtsNumber, "Int"),
      StackTestData(".zipWithIndex.length", "+", arrayInstance.length * MaxTtsNumber, "Int"),
      StackTestData(s".appended($firstLem).mkString", "+", (arrayInstance.mkString + firstInstance) * MaxTtsNumber, "String"),
      StackTestData(s".:+($firstLem).mkString", "+", arrayInstance.:+(firstInstance).mkString * MaxTtsNumber, "String"),
      StackTestData(s".:+($testArray).length", "+", arrayInstance.:+(arrayInstance).length * MaxTtsNumber, "Int"),
      StackTestData(s".:+[$lemTypeName]($firstLem).mkString", "+", arrayInstance.:+[T](firstInstance).mkString * MaxTtsNumber, "String"),
      StackTestData(s".prepended($firstLem).mkString", "+", (firstInstance.toString + arrayInstance.mkString) * MaxTtsNumber, "String"),
      StackTestData(s".+:($firstLem).mkString", "+", arrayInstance.+:(firstInstance).mkString * MaxTtsNumber, "String"),
      StackTestData(s".+:($testArray).length", "+", arrayInstance.+:(arrayInstance).length * MaxTtsNumber, "Int"),
      StackTestData(s".+:[$lemTypeName]($firstLem).mkString", "+", arrayInstance.+:[T](firstInstance).mkString * MaxTtsNumber, "String"),
      StackTestData(s".prependedAll(Iterator($firstLem)).mkString", "+", (firstInstance.toString + arrayInstance.mkString) * MaxTtsNumber, "String"),
      StackTestData(s".prependedAll($testArray).mkString", "+", arrayInstance.mkString * 2 * MaxTtsNumber, "String"),
      StackTestData(s".++:(Iterator($firstLem)).head.toString", "+", firstInstance.toString * MaxTtsNumber, "String"),
      StackTestData(s".++:($testArray).mkString", "+", arrayInstance.mkString * 2 * MaxTtsNumber, "String"),
      StackTestData(s".appendedAll(Iterator($firstLem)).mkString", "+", (arrayInstance.mkString + firstInstance) * MaxTtsNumber, "String"),
      StackTestData(s".appendedAll($testArray).mkString", "+", arrayInstance.mkString * 2 * MaxTtsNumber, "String"),
      StackTestData(s".:++(Iterator($firstLem)).last.toString", "+", firstInstance.toString * MaxTtsNumber, "String"),
      StackTestData(s".:++($testArray).mkString", "+", arrayInstance.mkString * 2 * MaxTtsNumber, "String"),
      StackTestData(s".concat(Iterator($firstLem)).last.toString", "+", firstInstance.toString * MaxTtsNumber, "String"),
      StackTestData(s".concat($testArray).mkString", "+", arrayInstance.mkString * 2 * MaxTtsNumber, "String"),
      StackTestData(s".++(Iterator($firstLem)).last.toString", "+", firstInstance.toString * MaxTtsNumber, "String"),
      StackTestData(s".++($testArray).mkString", "+", arrayInstance.mkString * 2 * MaxTtsNumber, "String"),
      StackTestData(s".contains($firstLem)", "&", true, "Boolean"),
      StackTestData(s".patch(0, $testArray, ${testArray.length}).mkString", "+", arrayInstance.mkString * MaxTtsNumber, "String"),
      StackTestData(s".unzip(l => (l, l), classTag[$lemTypeName], classTag[$lemTypeName])._1.mkString", "+", arrayInstance.mkString * MaxTtsNumber, "String"),
      StackTestData(s".unzip3(l => (l, l, l), classTag[$lemTypeName], classTag[$lemTypeName], classTag[$lemTypeName])._1.mkString", "+", arrayInstance.mkString * MaxTtsNumber, "String"),
      //Fails even in worksheet with java.lang.NullPointerException
      //  at java.lang.reflect.Array.newArray(Native Method)
      // StackTestData(s".transpose(l => Array[$lemTypeName](l))(0)(0).toString", "+", arrayInstance(0).toString * MaxTtsNumber, "String"),
      //Try foreach [U](f: AOT => U): Unit with source signature method foreach [U](f: A => U): Unit /erasure (f: Function1): Unit/ of class ArrayOps, isSynthetic=false
      StackTestData(".distinct.mkString", "+", arrayInstance.mkString * MaxTtsNumber, "String"),
      StackTestData(".distinctBy(identity(_)).mkString", "+", arrayInstance.mkString * MaxTtsNumber, "String"),
      StackTestData(s".padTo(4, $firstLem).mkString", "+", arrayInstance.padTo(4, firstInstance).mkString * MaxTtsNumber, "String"),
      StackTestData(".indices.head", "+", 0 * MaxTtsNumber, "Int"),
      StackTestData(".groupBy(_ => true)(true).mkString", "+", arrayInstance.mkString * MaxTtsNumber, "String"),
      StackTestData(".groupMap(_ => true)(identity(_)).apply(true).mkString", "+", arrayInstance.mkString * MaxTtsNumber, "String"),
      StackTestData(".toSeq.mkString", "+", arrayInstance.toSeq.mkString * MaxTtsNumber, "String"),
      StackTestData(".toIndexedSeq.mkString", "+", arrayInstance.mkString * MaxTtsNumber, "String"),
      StackTestData(s".copyToArray(new Array[$lemTypeName]($testArray.length))", "+", arrayInstance.length * MaxTtsNumber, "Int"),
      StackTestData(s".copyToArray(new Array[$lemTypeName]($testArray.length), 1)", "+", (arrayInstance.length - 1) * MaxTtsNumber, "Int"),
      StackTestData(s".copyToArray(new Array[$lemTypeName]($testArray.length), 0, 1)", "+", 1 * MaxTtsNumber, "Int"),
      StackTestData(".toArray[Any].length", "+", arrayInstance.length * MaxTtsNumber, "Int"),
      StackTestData(".count(_ => false)", "+", 0 * MaxTtsNumber, "Int"),
      StackTestData(s".startsWith($testArray)", "&", true, "Boolean"),
      StackTestData(s".startsWith($testArray, 1)", "|", false, "Boolean"),
      StackTestData(s".endsWith($testArray)", "&", true, "Boolean"),
      StackTestData(s".updated(0, $firstLem).mkString", "+", arrayInstance.updated(0, firstInstance).mkString * MaxTtsNumber, "String"),
      StackTestData(".view.length", "+", arrayInstance.length * MaxTtsNumber, "Int"),
      StackTestData(s".diff($testArray).mkString", "+", "" * MaxTtsNumber, "String"),
      StackTestData(s".intersect($testArray).mkString", "+", arrayInstance.mkString * MaxTtsNumber, "String"),
      StackTestData(".sliding(1).hasNext", "&", true, "Boolean"),
      StackTestData(".sliding(1, 1).hasNext", "&", true, "Boolean"),
      StackTestData(".combinations(2).length", "+", arrayInstance.combinations(2).length * MaxTtsNumber, "Int"),
      StackTestData(".permutations.length", "+", arrayInstance.permutations.length * MaxTtsNumber, "Int"),
      StackTestData(s".startsWith(Iterator($firstLem))", "&", true, "Boolean"),
      StackTestData(s".startsWith(Iterator($firstLem), 1)", "|", false, "Boolean"),
      StackTestData(s".endsWith($testArray.toSeq)", "&", true, "Boolean")
      //Try StackTestData(".hashCode()", "+", indexedSeqChar.hashCode() * MaxTtsNumber, "Int"),
      //Try method equals(that: Any)Boolean with signature equals(that: Any)Boolean /erasure (that: Object)Boolean/ of trait GenSeqLike, isSynthetic=false
    )

    // --- Array[T] extension by implicit conversion to WrappedArray[T] via wrap*Array implicits

    // to diff also uncomment ArrayOps at the end of this file
//    {
//      import sands.sugar.tms.TmsImplicitsCommon._
//      import sands.sugar.tms.replaceByPublicInterfacesDiff
//
//      import scala.annotation.compileTimeOnly
//      import scala.annotation.unchecked.uncheckedVariance
//      import scala.collection.ArrayOps
//
//      @replaceByPublicInterfacesDiff @compileTimeOnly(ctom) trait DebugWrappedArrayPartialInterface[AOT] {
//        // for parametrized trait the @uncheckedVariance ignores lots of variants errors (I see non actual)
//        // while applying type parameter value of TakeMembersOfType to it's methods of covariant typed parents, like:
//        // [error] covariant type AOT occurs in invariant position in type => scala.collection.parallel.mutable.ParArray[AOT] of method par
//        type TakeMembersOfType = ArraySeq[AOT/*@uncheckedVariance*/]
//        type DropMembersOfType = ArrayOps[AOT] @@ Object
//      }
//    }

    val implicitWrapArrayMethodName =
      if (lemTypeName == "Unit") "wrapUnitArray"
      else if (lemTypeName == "Any" || lemTypeName == "AnyVal") "genericWrapArray"
      else if (classIsAnyVal) "wrap" + lemTypeName + "Array"
      else "wrapRefArray"

    implicitExtensionOnTtsStacksTests(options, testValue = testArray, testValueType = s"Array[$lemTypeName]", typecheckedImplicitMethod = implicitWrapArrayMethodName, implicitType = s"ArraySeq[$lemTypeName]",
      // StackTestData(accessor: String, aggregator: String, result: Any, resultType: String, implicitConversionShouldExist: Boolean = true)
      // ArraySeq
      StackTestData(".elemTag.newArray(1).length", "+", arrayInstance.elemTag.newArray(1).length * MaxTtsNumber, "Int"),
      //Array native update (index: Int, elem: AOT): Unit with source signature method update (index: Int, elem: T): Unit /erasure (index: Int, elem: Object): Unit/ of class ArraySeq, isSynthetic=false
      StackTestData(s".array.asInstanceOf[Array[$lemTypeName]].mkString", "+", arrayInstance.mkString * MaxTtsNumber, "String"),
      StackTestData(".iterableFactory.newBuilder.result.mkString", "+", "" * MaxTtsNumber, "String"),
      StackTestData(".empty.length", "+", 0 * MaxTtsNumber, "Int"),
      //Try clone (): sands.sugar.tms.predef.ArraySeq[AOT] with source signature method clone (): sands.sugar.tms.predef.ArraySeq[T] /erasure (): sands.sugar.tms.predef.ArraySeq/ of class ArraySeq, isSynthetic=false
      StackTestData(".sortInPlace[Any]()(new Ordering[Any] {def compare(x: Any, y: Any) = 0}).mkString", "+", arrayInstance.clone.sortInPlace[Any]()(new Ordering[Any] {
        def compare(x: Any, y: Any) = 0
      }).mkString * MaxTtsNumber, "String"),
      // StrictOptimizedSeqOps
      //ArrayOps prepended [B >: AOT](elem: B): sands.sugar.tms.predef.ArraySeq[B] with source signature method prepended [B >: A](elem: B): CC[B] /erasure (elem: Object): sands.sugar.tms.predef.ArraySeq/ of trait StrictOptimizedSeqOps, isSynthetic=false
      //ArrayOps appended [B >: AOT](elem: B): sands.sugar.tms.predef.ArraySeq[B] with source signature method appended [B >: A](elem: B): CC[B] /erasure (elem: Object): sands.sugar.tms.predef.ArraySeq/ of trait StrictOptimizedSeqOps, isSynthetic=false
      //ArrayOps appendedAll [B >: AOT](suffix: scala.collection.IterableOnce[B]): sands.sugar.tms.predef.ArraySeq[B] with source signature method appendedAll [B >: A](suffix: scala.collection.IterableOnce[B]): CC[B] /erasure (suffix: scala.collection.IterableOnce): sands.sugar.tms.predef.ArraySeq/ of trait StrictOptimizedSeqOps, isSynthetic=false
      //ArrayOps prependedAll [B >: AOT](prefix: scala.collection.IterableOnce[B]): sands.sugar.tms.predef.ArraySeq[B] with source signature method prependedAll [B >: A](prefix: scala.collection.IterableOnce[B]): CC[B] /erasure (prefix: scala.collection.IterableOnce): sands.sugar.tms.predef.ArraySeq/ of trait StrictOptimizedSeqOps, isSynthetic=false
      //ArrayOps padTo [B >: AOT](len: Int, elem: B): sands.sugar.tms.predef.ArraySeq[B] with source signature method padTo [B >: A](len: Int, elem: B): CC[B] /erasure (len: Int, elem: Object): sands.sugar.tms.predef.ArraySeq/ of trait StrictOptimizedSeqOps, isSynthetic=false
      //ArrayOps intersect [B >: AOT](that: scala.collection.Seq[B]): sands.sugar.tms.predef.ArraySeq[AOT] with source signature method intersect [B >: A](that: scala.collection.Seq[B]): C /erasure (that: scala.collection.Seq): sands.sugar.tms.predef.ArraySeq/ of trait StrictOptimizedSeqOps, isSynthetic=false
      // StrictOptimizedIterableOps
      //ArrayOps unzip [A1, A2](implicit asPair: AOT => (A1, A2)): (sands.sugar.tms.predef.ArraySeq[A1], sands.sugar.tms.predef.ArraySeq[A2]) with source signature method unzip [A1, A2](implicit asPair: A => (A1, A2)): (CC[A1], CC[A2]) /erasure (asPair: Function1): Tuple2/ of trait StrictOptimizedIterableOps, isSynthetic=false
      //ArrayOps unzip3 [A1, A2, A3](implicit asTriple: AOT => (A1, A2, A3)): (sands.sugar.tms.predef.ArraySeq[A1], sands.sugar.tms.predef.ArraySeq[A2], sands.sugar.tms.predef.ArraySeq[A3]) with source signature method unzip3 [A1, A2, A3](implicit asTriple: A => (A1, A2, A3)): (CC[A1], CC[A2], CC[A3]) /erasure (asTriple: Function1): Tuple3/ of trait StrictOptimizedIterableOps, isSynthetic=false
      //Try map [B](f: AOT => B): sands.sugar.tms.predef.ArraySeq[B] with source signature method map [B](f: A => B): CC[B] /erasure (f: Function1): sands.sugar.tms.predef.ArraySeq/ of trait StrictOptimizedIterableOps, isSynthetic=false
      //Try flatMap [B](f: AOT => scala.collection.IterableOnce[B]): sands.sugar.tms.predef.ArraySeq[B] with source signature method flatMap [B](f: A => scala.collection.IterableOnce[B]): CC[B] /erasure (f: Function1): sands.sugar.tms.predef.ArraySeq/ of trait StrictOptimizedIterableOps, isSynthetic=false
      //Try collect [B](pf: PartialFunction[AOT,B]): sands.sugar.tms.predef.ArraySeq[B] with source signature method collect [B](pf: PartialFunction[A,B]): CC[B] /erasure (pf: PartialFunction): sands.sugar.tms.predef.ArraySeq/ of trait StrictOptimizedIterableOps, isSynthetic=false
      //Try flatten [B](implicit toIterableOnce: AOT => scala.collection.IterableOnce[B]): sands.sugar.tms.predef.ArraySeq[B] with source signature method flatten [B](implicit toIterableOnce: A => scala.collection.IterableOnce[B]): CC[B] /erasure (toIterableOnce: Function1): sands.sugar.tms.predef.ArraySeq/ of trait StrictOptimizedIterableOps, isSynthetic=false
      //ArrayOps scanLeft [B](z: B)(op: (B, AOT) => B): sands.sugar.tms.predef.ArraySeq[B] with source signature method scanLeft [B](z: B)(op: (B, A) => B): CC[B] /erasure (z: Object, op: Function2): sands.sugar.tms.predef.ArraySeq/ of trait StrictOptimizedIterableOps, isSynthetic=false
      //ArrayOps partitionMap [A1, A2](f: AOT => Either[A1,A2]): (sands.sugar.tms.predef.ArraySeq[A1], sands.sugar.tms.predef.ArraySeq[A2]) with source signature method partitionMap [A1, A2](f: A => Either[A1,A2]): (CC[A1], CC[A2]) /erasure (f: Function1): Tuple2/ of trait StrictOptimizedIterableOps, isSynthetic=false
      StackTestData(s".tapEach(_ => '0').mkString", "+", arrayInstance.mkString * MaxTtsNumber, "String"),
      // IndexedSeqOps
      StackTestData(".sortInPlaceWith((a, b) => true).mkString", "+", arrayInstance.clone.sortInPlaceWith((a, b) => true).mkString * MaxTtsNumber, "String"),
      StackTestData(".sortInPlaceBy(_ => 0).mkString", "+", arrayInstance.clone.sortInPlaceBy(_ => 0).mkString * MaxTtsNumber, "String"),
      //deprecated & doesn't compile even without implicits: StackTestData(".view(0, 1).iterator.mkString", "+", firstInstance.toString * MaxTtsNumber, "String"),
      //doesn't compile even without implicits: StackTestData(".lengthCompare(Seq())", "+", arrayInstance.lengthCompare(Seq()) * MaxTtsNumber, "Int"),
      StackTestData(s".search[Any]($firstLem)(new Ordering[Any] {def compare(x: Any, y: Any) = 0}).insertionPoint", "+", arrayInstance.search[Any](firstInstance)(new Ordering[Any] {
        def compare(x: Any, y: Any) = 0
      }).insertionPoint * MaxTtsNumber, "Int"),
      StackTestData(s".search[Any]($firstLem, 1, 2)(new Ordering[Any] {def compare(x: Any, y: Any) = 0}).insertionPoint", "+", arrayInstance.search[Any](firstInstance, 1, 2)(new Ordering[Any] {
        def compare(x: Any, y: Any) = 0
      }).insertionPoint * MaxTtsNumber, "Int"),
      // SeqOps
      //Try StackTestData(".transform(identity(_)).mkString", "+", arrayInstance.mkString * MaxTtsNumber, "String"),
      //Try 2.12+ StackTestData(s".canEqual($testArray)", "&", true, "Boolean"),
      //Array native apply (i: Int): AOT with source signature method apply (i: Int): A /erasure (i: Int): Object/ of trait SeqOps, isSynthetic=false
      //Array native length Int with source signature method length Int /erasure (): Int/ of trait SeqOps, isSynthetic=false
      //ArrayOps $plus$colon [B >: AOT](elem: B): sands.sugar.tms.predef.ArraySeq[B] with source signature method +: [B >: A](elem: B): CC[B] /erasure (elem: Object): sands.sugar.tms.predef.ArraySeq/ of trait SeqOps, isSynthetic=false
      //ArrayOps $colon$plus [B >: AOT](elem: B): sands.sugar.tms.predef.ArraySeq[B] with source signature method :+ [B >: A](elem: B): CC[B] /erasure (elem: Object): sands.sugar.tms.predef.ArraySeq/ of trait SeqOps, isSynthetic=false
      //ArrayOps $plus$plus$colon [B >: AOT](prefix: scala.collection.IterableOnce[B]): sands.sugar.tms.predef.ArraySeq[B] with source signature method ++: [B >: A](prefix: scala.collection.IterableOnce[B]): CC[B] /erasure (prefix: scala.collection.IterableOnce): sands.sugar.tms.predef.ArraySeq/ of trait SeqOps, isSynthetic=false
      //ArrayOps $colon$plus$plus [B >: AOT](suffix: scala.collection.IterableOnce[B]): sands.sugar.tms.predef.ArraySeq[B] with source signature method :++ [B >: A](suffix: scala.collection.IterableOnce[B]): CC[B] /erasure (suffix: scala.collection.IterableOnce): sands.sugar.tms.predef.ArraySeq/ of trait SeqOps, isSynthetic=false
      //ArrayOps concat [B >: AOT](suffix: scala.collection.IterableOnce[B]): sands.sugar.tms.predef.ArraySeq[B] with source signature method concat [B >: A](suffix: scala.collection.IterableOnce[B]): CC[B] /erasure (suffix: scala.collection.IterableOnce): sands.sugar.tms.predef.ArraySeq/ of trait SeqOps, isSynthetic=false
      StackTestData(s".union($testArray).mkString", "+", arrayInstance.mkString * 2 * MaxTtsNumber, "String"),
      StackTestData(".isDefinedAt(-1)", "|", false, "Boolean"),
      StackTestData(s".segmentLength(_ => true)", "+", arrayInstance.length * MaxTtsNumber, "Int"),
      StackTestData(s".segmentLength(_ => true, 0)", "+", arrayInstance.length * MaxTtsNumber, "Int"),
      StackTestData(".prefixLength(_ => true)", "+", arrayInstance.length * MaxTtsNumber, "Int"),
      //ArrayOps indexWhere (p: AOT => Boolean): Int with source signature method indexWhere (p: A => Boolean): Int /erasure (p: Function1): Int/ of trait SeqOps, isSynthetic=false
      //ArrayOps indexOf [B >: AOT](elem: B): Int with source signature method indexOf [B >: A](elem: B): Int /erasure (elem: Object): Int/ of trait SeqOps, isSynthetic=false
      //ArrayOps lastIndexWhere (p: AOT => Boolean): Int with source signature method lastIndexWhere (p: A => Boolean): Int /erasure (p: Function1): Int/ of trait SeqOps, isSynthetic=false
      StackTestData(s".indexOfSlice($testArray, 1)", "+", -1 * MaxTtsNumber, "Int"),
      StackTestData(s".indexOfSlice($testArray)", "+", 0 * MaxTtsNumber, "Int"),
      StackTestData(s".lastIndexOfSlice($testArray, 1)", "+", 0 * MaxTtsNumber, "Int"),
      StackTestData(s".lastIndexOfSlice($testArray)", "+", 0 * MaxTtsNumber, "Int"),
      StackTestData(".findLast(_ => false).isEmpty", "&", true, "Boolean"),
      StackTestData(s".containsSlice($testArray)", "&", true, "Boolean"),
      StackTestData(".reverseMap(identity(_)).mkString", "+", arrayInstance.reverseMap(identity(_)).mkString * MaxTtsNumber, "String"),
      //doesn't compile even without implicits: StackTestData(".sizeCompare(Seq())", "+", arrayInstance.sizeCompare(Seq()) * MaxTtsNumber, "Int"),
      StackTestData(s".sameElements($testArray.toSeq)", "&", true, "Boolean"),
      StackTestData(s".corresponds($testArray)((a, b) => true)", "&", true, "Boolean"),
      StackTestData(s".patch(0, Iterator($firstLem), 1).mkString", "+", arrayInstance.mkString * MaxTtsNumber, "String"),
      // PartialFunction
      StackTestData(".unapply(0).get.toString", "+", firstInstance.toString * MaxTtsNumber, "String"),
      StackTestData(".elementWise.unapplySeq(Seq(0)).get.head.toString", "+", firstInstance.toString * MaxTtsNumber, "String"),
      //Try method orElse[A1 <: A, B1 >: B](that: PartialFunction[A1,B1])PartialFunction[A1,B1] with signature orElse[A1 <: Int, B1 >: Any](that: PartialFunction[A1,B1])PartialFunction[A1,B1] /erasure (that: PartialFunction)PartialFunction/ of trait PartialFunction, isSynthetic=false
      //Try method andThen[C](k: B => C)PartialFunction[A,C] with signature andThen[C](k: Any => C)PartialFunction[Int,C] /erasure (k: Function1)PartialFunction/ of trait PartialFunction, isSynthetic=false
      StackTestData(".compose[Int]{case i: Int => i}.apply(0).toString", "+", arrayInstance.head.toString * MaxTtsNumber, "String"),
      StackTestData(".lift(0)", "orElse", arrayInstance.headOption, s"Option[$lemTypeName]"),
      StackTestData(s".applyOrElse(0, (_: Int) => $firstInstance).toString", "+", arrayInstance.head.toString * MaxTtsNumber, "String"),
      StackTestData(".runWith(identity(_))(0)", "&", true, "Boolean"),
      // Function1
      // 2.13.5 compiler generates PartialFunction (instead of Function) with wrong code error on macros lifting StackTestData(".compose[Int](identity[Int](_)).apply(0).toString", "+", arrayInstance.head.toString * MaxTtsNumber, "String"),
      StackTestData(".compose[Int](identity[Int] _).apply(0).toString", "+", arrayInstance.head.toString * MaxTtsNumber, "String"),
      // Iterable
      StackTestData(".toIterable.iterator.hasNext", "&", true, "Boolean"),
      StackTestData(".seq.mkString", "+", arrayInstance.mkString * MaxTtsNumber, "String"),
      // IterableOps
      StackTestData(".toTraversable.mkString", "+", arrayInstance.mkString * MaxTtsNumber, "String"),
      StackTestData(".isTraversableAgain", "&", true, "Boolean"),
      StackTestData(".repr.mkString", "+", arrayInstance.mkString * MaxTtsNumber, "String"),
      StackTestData(".companion.empty.mkString", "+", arrayInstance.companion.empty.mkString * MaxTtsNumber, "String"),
      //ArrayOps sliding (size: Int): Iterator[sands.sugar.tms.predef.ArraySeq[AOT]] with source signature method sliding (size: Int): Iterator[C] /erasure (size: Int): Iterator/ of trait IterableOps, isSynthetic=false
      //ArrayOps groupMap [K, B](key: AOT => K)(f: AOT => B): scala.collection.immutable.Map[K,sands.sugar.tms.predef.ArraySeq[B]] with source signature method groupMap [K, B](key: A => K)(f: A => B): scala.collection.immutable.Map[K,CC[B]] /erasure (key: Function1, f: Function1): scala.collection.immutable.Map/ of trait IterableOps, isSynthetic=false
      StackTestData(".groupMapReduce(_ => true)(identity(_))((c1, c2) => c1).apply(true).toString", "+", firstInstance.toString * MaxTtsNumber, "String"),
      //ArrayOps scan [B >: AOT](z: B)(op: (B, B) => B): sands.sugar.tms.predef.ArraySeq[B] with source signature method scan [B >: A](z: B)(op: (B, B) => B): CC[B] /erasure (z: Object, op: Function2): sands.sugar.tms.predef.ArraySeq/ of trait IterableOps, isSynthetic=false
      //ArrayOps scanRight [B](z: B)(op: (AOT, B) => B): sands.sugar.tms.predef.ArraySeq[B] with source signature method scanRight [B](z: B)(op: (A, B) => B): CC[B] /erasure (z: Object, op: Function2): sands.sugar.tms.predef.ArraySeq/ of trait IterableOps, isSynthetic=false
      //ArrayOps $plus$plus [B >: AOT](suffix: scala.collection.IterableOnce[B]): sands.sugar.tms.predef.ArraySeq[B] with source signature method ++ [B >: A](suffix: scala.collection.IterableOnce[B]): CC[B] /erasure (suffix: scala.collection.IterableOnce): sands.sugar.tms.predef.ArraySeq/ of trait IterableOps, isSynthetic=false
      // IterableOps
      StackTestData(".hasDefiniteSize", "&", true, "Boolean"),
      StackTestData(s"./:($firstLem)((c1, c2) => c1).toString", "+", firstInstance.toString * MaxTtsNumber, "String"),
      StackTestData(s".:\\($firstLem)((c1, c2) => c2).toString", "+", firstInstance.toString * MaxTtsNumber, "String"),
      StackTestData(".reduce((c1, c2) => c1).toString", "+", arrayInstance.head.toString * MaxTtsNumber, "String"),
      StackTestData(".reduceOption((c1, c2) => c1).nonEmpty", "&", true, "Boolean"),
      StackTestData(s".reduceLeft((b, t) => b).toString", "+", arrayInstance.reduceLeft((b, t) => b).toString * MaxTtsNumber, "String"),
      StackTestData(s".reduceRight((b, t) => b).toString", "+", arrayInstance.reduceRight((b, t) => b).toString * MaxTtsNumber, "String"),
      StackTestData(".reduceLeftOption((c1, c2) => c1).nonEmpty", "&", true, "Boolean"),
      StackTestData(".reduceRightOption((c1, c2) => c2).nonEmpty", "&", true, "Boolean"),
      StackTestData(s".copyToBuffer(scala.collection.mutable.Buffer[$lemTypeName]()).toString", "+", "()" * MaxTtsNumber, "String"),
      //Numeric[T] StackTestData(".sum.toString", "+", arrayInstance.sum.toString * MaxTtsNumber, "String"),
      //Numeric[T] StackTestData(".product.toString", "+", arrayInstance.product.toString * MaxTtsNumber, "String"),
      //Rich*.min(n) StackTestData(".min(new Ordering[Any] {def compare(x: Any, y: Any) = 0}).toString", "+", arrayInstance.min(new Ordering[Any] {
      //   def compare(x: Any, y: Any) = 0
      //}).toString * MaxTtsNumber, "String"),
      StackTestData(".minOption(new Ordering[Any] {def compare(x: Any, y: Any) = 0}).get.toString", "+", arrayInstance.minOption(new Ordering[Any] {
        def compare(x: Any, y: Any) = 0
      }).get.toString * MaxTtsNumber, "String"),
      //Rich*.max(n) StackTestData(".max(new Ordering[Any] {def compare(x: Any, y: Any) = 0}).toString", "+", arrayInstance.max(new Ordering[Any] {
      //  def compare(x: Any, y: Any) = 0
      //}).toString * MaxTtsNumber, "String"),
      StackTestData(".maxOption(new Ordering[Any] {def compare(x: Any, y: Any) = 0}).get.toString", "+", arrayInstance.maxOption(new Ordering[Any] {
        def compare(x: Any, y: Any) = 0
      }).get.toString * MaxTtsNumber, "String"),
      StackTestData(".maxBy(_.toString).toString", "+", arrayInstance.maxBy(_.toString).toString * MaxTtsNumber, "String"),
      StackTestData(".maxByOption(_.toString).get.toString", "+", arrayInstance.maxByOption(_.toString).get.toString * MaxTtsNumber, "String"),
      StackTestData(".minBy(_.toString).toString", "+", arrayInstance.minBy(_.toString).toString * MaxTtsNumber, "String"),
      StackTestData(".minByOption(_.toString).get.toString", "+", arrayInstance.minByOption(_.toString).get.toString * MaxTtsNumber, "String"),
      StackTestData(s".aggregate($firstLem)((c1, c2) => c1, (c1, c2) => c1).toString", "+", arrayInstance.aggregate(firstInstance)((c1, c2) => c1, (c1, c2) => c1).toString * MaxTtsNumber, "String"),
      StackTestData(s".corresponds($testArray.iterator)((a, b) => true)", "&", true, "Boolean"),
      StackTestData(""".mkString("<", "-", ">")""", "+", arrayInstance.mkString("<", "-", ">") * MaxTtsNumber, "String"),
      StackTestData(""".mkString("-")""", "+", arrayInstance.mkString("-") * MaxTtsNumber, "String"),
      StackTestData(".mkString", "+", arrayInstance.mkString * MaxTtsNumber, "String"),
      StackTestData(""".addString(new StringBuilder, "<", "-", ">").toString""", "+", arrayInstance.mkString("<", "-", ">") * MaxTtsNumber, "String"),
      StackTestData(""".addString(new StringBuilder, "-").toString""", "+", arrayInstance.mkString("-") * MaxTtsNumber, "String"),
      StackTestData(""".addString(new StringBuilder).toString""", "+", arrayInstance.mkString * MaxTtsNumber, "String"),
      //Rich*.to(n) StackTestData(s".to[Seq[$lemTypeName]](Seq).mkString", "+", arrayInstance.mkString * MaxTtsNumber, "String"),
      StackTestData(".toIterator.hasNext", "&", true, "Boolean"),
      StackTestData(".toList.mkString", "+", arrayInstance.mkString * MaxTtsNumber, "String"),
      StackTestData(".toVector.mkString", "+", arrayInstance.mkString * MaxTtsNumber, "String"),
      //Tuple2 method toMap[T, U](implicit ev: <:<[A,(T, U)])scala.collection.immutable.Map[T,U] with signature toMap[T, U](implicit ev: <:<[Any @scala.annotation.unchecked.uncheckedVariance,(T, U)])scala.collection.immutable.Map[T,U] /erasure (ev: <:<)scala.collection.immutable.Map/ of trait TraversableOnce, isSynthetic=false
      StackTestData(".toSet", "++", arrayInstance.toSet, s"Set[$lemTypeName]"),
      StackTestData(".toStream.mkString", "+", arrayInstance.mkString * MaxTtsNumber, "String"),
      StackTestData(".toBuffer.mkString", "+", arrayInstance.toBuffer.mkString * MaxTtsNumber, "String")
    )
  }
}

  // non-sealed helper copy of scala.collection.mutable.ArraySeq interface to diff ArraySeq[T] - ArrayOps[T]
//import scala.collection.Stepper.EfficientSplit
//import scala.collection.{Stepper, StepperShape, StrictOptimizedSeqOps}
//import scala.collection.mutable.{AbstractSeq, Builder, IndexedSeq, IndexedSeqOps}
//
//@SerialVersionUID(3L)
//abstract class ArraySeq[T]
//  extends AbstractSeq[T]
//    with IndexedSeq[T]
//    with IndexedSeqOps[T, ArraySeq, ArraySeq[T]]
//    with StrictOptimizedSeqOps[T, ArraySeq, ArraySeq[T]]
//    with Serializable {
//
//  override def iterableFactory: scala.collection.SeqFactory[ArraySeq] = ???
//
//  override protected def fromSpecific(coll: scala.collection.IterableOnce[T]): ArraySeq[T] = ???
//  override protected def newSpecificBuilder: Builder[T, ArraySeq[T]] = ???
//  override def empty: ArraySeq[T] = ???
//
//  def elemTag: ClassTag[_]
//
//  /** Update element at given index */
//  def update(@deprecatedName("idx", "2.13.0") index: Int, elem: T): Unit
//
//  def array: Array[_]
//
//  override def stepper[S <: Stepper[_]](implicit shape: StepperShape[T, S]): S with EfficientSplit = ???
//
//  override protected[this] def className = "ArraySeq"
//
//  override def clone(): ArraySeq[T] = ???
//
//  override def copyToArray[B >: T](xs: Array[B], start: Int, len: Int): Int = ???
//
//  override def equals(other: Any): Boolean = ???
//
//  override def sorted[B >: T](implicit ord: Ordering[B]): ArraySeq[T] = ???
//
//  override def sortInPlace[B >: T]()(implicit ord: Ordering[B]): this.type = ???
//}
