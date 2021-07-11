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
import sands.sugar.tms.TmsForTestOptions._

/*
 * Created by Serhiy Shamshetdinov
 * at 18.02.2021 16:53
 */

class TmsForStringImplicitMethodsTest extends TmsTestBase {

  stringImplicitMethodsTests(SimpleForsStackOptions)

  def stringImplicitMethodsTests(tmsOptions: Seq[String]): Unit = {
    val options = literalOptions(tmsOptions)

    behavior of "tms macros Scala Predef String implicit methods implicits with TmsOptions" + options

    // --- String extension by implicit conversion to StringOps via augmentString

//    {
//      import sands.sugar.tms.TmsImplicitsCommon.ctom
//      import sands.sugar.tms.replaceByPublicInterfacesDiff
//
//      import scala.annotation.compileTimeOnly
//
//      @replaceByPublicInterfacesDiff @compileTimeOnly(ctom) trait DebugWrappedStringPartialInterface {
//        type TakeMembersOfType = scala.collection.immutable.StringOps
//        type DropMembersOfType = String
//      }
//    }

    implicitExtensionOnTtsStacksTests(options, testValue = "\"123\"", testValueType = "String", typecheckedImplicitMethod = "augmentString", implicitType = "StringOps",
      // StackTestData(accessor: String, aggregator: String, result: Any, resultType: String, implicitConversionShouldExist: Boolean = true)
      // StringOps
      StackTestData(".view.length", "+", 3 * MaxTtsNumber, "Int"),
      StackTestData(".size", "+", 3 * MaxTtsNumber, "Int"),
      StackTestData(".knownSize", "+", 3 * MaxTtsNumber, "Int"),
      StackTestData("(0)", "+", '1' * MaxTtsNumber, "Int"),
      StackTestData(".apply(1)", "+", '2' * MaxTtsNumber, "Int"),
      StackTestData(".sizeCompare(3)", "+", 0 * MaxTtsNumber, "Int"),
      StackTestData(".lengthCompare(3)", "+", 0 * MaxTtsNumber, "Int"),
      StackTestData(".sizeIs", "+", 3 * MaxTtsNumber, "Int"),
      StackTestData(".lengthIs", "+", 3 * MaxTtsNumber, "Int"),
      //Try      StackTestData(".map(_ - '0').sum", "+",  6 * MaxTtsNumber, "Int"),
      //Try      StackTestData(".flatMap(_ => '0')", "+",  "000" * MaxTtsNumber, "String"),
      //Try 2.12 StackTestData(".collect{case c if c > '2' => c}", "+", "3" * MaxTtsNumber, "String"),
      StackTestData(".concat[AnyVal](Iterator[AnyVal]('4')).mkString", "+", "1234" * MaxTtsNumber, "String"),
      StackTestData(".concat(Iterator('4'))", "+", "1234" * MaxTtsNumber, "String"),
      StackTestData(".++[Char](Iterable('4')).mkString", "+", "1234" * MaxTtsNumber, "String"),
      StackTestData(".++[AnyVal](Iterable[AnyVal]('4')).mkString", "+", "1234" * MaxTtsNumber, "String"),
      StackTestData(".++(Iterator('4'))", "+", "1234" * MaxTtsNumber, "String"),
      StackTestData(".++(\"45\")", "+", "12345" * MaxTtsNumber, "String"),
      StackTestData(".padTo[Any](4, '4'.toInt: Any).mkString", "+", "12352" * MaxTtsNumber, "String"),
      StackTestData(".padTo(4, '4')", "+", "1234" * MaxTtsNumber, "String"),
      StackTestData(".prepended[Any]('4'.toInt: Any).mkString", "+", "52123" * MaxTtsNumber, "String"),
      StackTestData(".+:[Any]('4'.toInt: Any).mkString", "+", "52123" * MaxTtsNumber, "String"),
      StackTestData(".prepended('4').mkString", "+", "4123" * MaxTtsNumber, "String"),
      StackTestData(".+:('4').mkString", "+", "4123" * MaxTtsNumber, "String"),
      StackTestData(".prependedAll[Any](Iterator[Any]('0')).mkString", "+", "0123" * MaxTtsNumber, "String"),
      StackTestData(".++:[Any](Iterator[Any]('0')).mkString", "+", "0123" * MaxTtsNumber, "String"),
      StackTestData(".prependedAll(\"00\")", "+", "00123" * MaxTtsNumber, "String"),
      StackTestData(".++:(\"00\")", "+", "00123" * MaxTtsNumber, "String"),
      StackTestData(".appended[Any]('4'.toInt: Any).mkString", "+", "12352" * MaxTtsNumber, "String"),
      StackTestData(".:+[Any]('4'.toInt: Any).mkString", "+", "12352" * MaxTtsNumber, "String"),
      StackTestData(".appended('4')", "+", "1234" * MaxTtsNumber, "String"),
      StackTestData(".:+('4')", "+", "1234" * MaxTtsNumber, "String"),
      StackTestData(".appendedAll[Any](Iterator[Any]('4', '5')).mkString", "+", "12345" * MaxTtsNumber, "String"),
      StackTestData(".:++[Any](Iterator[Any]('4', '5')).mkString", "+", "12345" * MaxTtsNumber, "String"),
      StackTestData(".appendedAll(\"45\")", "+", "12345" * MaxTtsNumber, "String"),
      StackTestData(".:++(\"45\")", "+", "12345" * MaxTtsNumber, "String"),
      StackTestData(".patch[Any](1, Iterator[Any]('4'.toInt, '5'.toInt), 1).mkString", "+", "152533" * MaxTtsNumber, "String"),
      StackTestData(".patch(1, Iterator('4', '5'), 1)", "+", "1453" * MaxTtsNumber, "String"),
      StackTestData(".patch(1, \"45\", 1)", "+", "1453" * MaxTtsNumber, "String"),
      StackTestData(".updated(1, '4').mkString", "+", "143" * MaxTtsNumber, "String"),
      StackTestData(".contains('3')", "&", true, "Boolean"),
      StackTestData(""".mkString("<", "-", ">")""", "+", "<1-2-3>" * MaxTtsNumber, "String"),
      StackTestData(""".mkString("-")""", "+", "1-2-3" * MaxTtsNumber, "String"),
      StackTestData(".mkString", "+", "123" * MaxTtsNumber, "String"),
      StackTestData(""".addString(new StringBuilder).toString""", "+", "123" * MaxTtsNumber, "String"),
      StackTestData(""".addString(new StringBuilder, "-").toString""", "+", "1-2-3" * MaxTtsNumber, "String"),
      StackTestData(""".addString(new StringBuilder, "<", "-", ">").toString""", "+", "<1-2-3>" * MaxTtsNumber, "String"),
      StackTestData(".slice(1, 2)", "+", "2" * MaxTtsNumber, "String"),
      StackTestData("* 2", "+", "123123" * MaxTtsNumber, "String"),
      StackTestData(".stripLineEnd", "+", "123" * MaxTtsNumber, "String"),
      StackTestData(".linesWithSeparators.next()", "+", "123" * MaxTtsNumber, "String"),
      StackTestData(".linesIterator.next()", "+", "123" * MaxTtsNumber, "String"),
      //deprecated StackTestData(".lines.next()", "+", "123" * MaxTtsNumber, "String"),
      StackTestData(".capitalize.toString", "+", "123" * MaxTtsNumber, "String"),
      StackTestData(".stripPrefix(\"12\")", "+", "3" * MaxTtsNumber, "String"),
      StackTestData(".stripSuffix(\"23\")", "+", "1" * MaxTtsNumber, "String"),
      StackTestData(".replaceAllLiterally(\"23\", \"0\")", "+", "10" * MaxTtsNumber, "String"),
      StackTestData(".stripMargin", "+", "123" * MaxTtsNumber, "String"),
      StackTestData(".stripMargin('1')", "+", "23" * MaxTtsNumber, "String"),
      StackTestData(".split('2').length", "+", 2 * MaxTtsNumber, "Int"),
      StackTestData(".split(Array[Char]('2')).length", "+", 2 * MaxTtsNumber, "Int"),
      StackTestData(".r.regex", "+", "123" * MaxTtsNumber, "String"),
      StackTestData(".r().regex", "+", "123" * MaxTtsNumber, "String"),
      StackTestData(".r(\"test\").regex", "+", "123" * MaxTtsNumber, "String"),
      //Exception StackTestData(".toBoolean", "|", false, "Boolean"),
      StackTestData(".toBooleanOption", "orElse", None, "Option[Boolean]"),
      StackTestData(".toByte", "+", 123 * MaxTtsNumber, "Int"),
      StackTestData(".toByteOption", "orElse", Some(123: Byte), "Option[Byte]"),
      StackTestData(".toShort", "+", 123 * MaxTtsNumber, "Int"),
      StackTestData(".toShortOption", "orElse", Some(123: Short), "Option[Short]"),
      StackTestData(".toInt", "+", 123 * MaxTtsNumber, "Int"),
      StackTestData(".toIntOption", "orElse", Some(123), "Option[Int]"),
      StackTestData(".toLong", "+", 123L * MaxTtsNumber, "Long"),
      StackTestData(".toLongOption", "orElse", Some(123L), "Option[Long]"),
      StackTestData(".toFloat", "+", 123F * MaxTtsNumber, "Float"),
      StackTestData(".toFloatOption", "orElse", Some(123F), "Option[Float]"),
      StackTestData(".toDouble", "+", 123D * MaxTtsNumber, "Double"),
      StackTestData(".toDoubleOption", "orElse", Some(123D), "Option[Double]"),
      StackTestData(".toArray[Char].length", "+", 3 * MaxTtsNumber, "Int"),
      StackTestData(".format()", "+", "123" * MaxTtsNumber, "String"),
      StackTestData(".formatLocal(java.util.Locale.US)", "+", "123" * MaxTtsNumber, "String"),
      StackTestData(".compare(\"123\")", "+", 0, "Int"),
      StackTestData("< \"2\" ", "&", true, "Boolean"),
      StackTestData("> \"0\" ", "&", true, "Boolean"),
      StackTestData("<= \"2\" ", "&", true, "Boolean"),
      StackTestData(">= \"0\" ", "&", true, "Boolean"),
      StackTestData(".count(_ > '1')", "+", 2 * MaxTtsNumber, "Int"),
      //Try  foreach()
      StackTestData(".forall(_ > '0')", "&", true, "Boolean"),
      StackTestData(".foldLeft(0)(_ + _)", "+", "123".foldLeft(0)(_ + _) * MaxTtsNumber, "Int"),
      StackTestData(".foldRight(0)(_ + _)", "+", "123".foldRight(0)(_ + _) * MaxTtsNumber, "Int"),
      //Try 2.12 StackTestData(".fold('0')((c1: Char, c2: Char) => (c1 + c2 - '0').toChar)", "+", '6' * MaxTtsNumber, "Int"),
      StackTestData(".head", "+", '1' * MaxTtsNumber, "Int"),
      StackTestData(".headOption", "orElse", Some('1'), "Option[Char]"),
      StackTestData(".last", "+", '3' * MaxTtsNumber, "Int"),
      StackTestData(".lastOption", "orElse", Some('3'), "Option[Char]"),
      StackTestData(".indices.last", "+", 2 * MaxTtsNumber, "Int"),
      StackTestData(".iterator.next", "+", '1' * MaxTtsNumber, "Int"),
      StackTestData(".stepper.hasStep", "&", true, "Boolean"),
      StackTestData(".charStepper.hasStep", "&", true, "Boolean"),
      StackTestData(".codePointStepper.hasStep", "&", true, "Boolean"),
      StackTestData(".nonEmpty", "&", true, "Boolean"),
      StackTestData(".reverse", "+", "321" * MaxTtsNumber, "String"),
      StackTestData(".reverseIterator.next", "+", '3' * MaxTtsNumber, "Int"),
      //Try      StackTestData(".withFilter(_ == '1').mkString", "+",  "1" * MaxTtsNumber, "String"),
      StackTestData(".tail", "+", "23" * MaxTtsNumber, "String"),
      StackTestData(".init", "+", "12" * MaxTtsNumber, "String"),
      StackTestData(".take(2)", "+", "12" * MaxTtsNumber, "String"),
      StackTestData(".drop(1)", "+", "23" * MaxTtsNumber, "String"),
      StackTestData(".takeRight(2)", "+", "23" * MaxTtsNumber, "String"),
      StackTestData(".dropRight(2)", "+", "1" * MaxTtsNumber, "String"),
      StackTestData(".tails.next", "+", "123" * MaxTtsNumber, "String"),
      StackTestData(".inits.next", "+", "123" * MaxTtsNumber, "String"),
      //Try      StackTestData(".filter(_ >= '2')", "+",  "23" * MaxTtsNumber, "String"),
      StackTestData(".filterNot(_ >= '2')", "+",  "1" * MaxTtsNumber, "String"),
      StackTestData(".copyToArray(Array[Char](3)).toString", "+", "1" * MaxTtsNumber, "String"),
      StackTestData(".copyToArray(Array[Char](2), 1).toString", "+", "0" * MaxTtsNumber, "String"),
      StackTestData(".copyToArray(new Array[Char](1), 1, 1).toString", "+", "0" * MaxTtsNumber, "String"),
      StackTestData(".indexWhere(_ >= '2')", "+", 1 * MaxTtsNumber, "Int"),
      StackTestData(".indexWhere(_ > '0', 1)", "+", 1 * MaxTtsNumber, "Int"),
      StackTestData(".lastIndexWhere(_ >= '2')", "+", 2 * MaxTtsNumber, "Int"),
      StackTestData(".lastIndexWhere(_ > '0', 1)", "+", 1 * MaxTtsNumber, "Int"),
      StackTestData(".exists(_ == '1')", "&", true, "Boolean"),
      StackTestData(".find(_ > '2')", "orElse", Some('3'), "Option[Char]"),
      StackTestData(".dropWhile(_ <= '2')", "+", "3" * MaxTtsNumber, "String"),
      StackTestData(".takeWhile(_ <= '2')", "+", "12" * MaxTtsNumber, "String"),
      StackTestData(".splitAt(1)._2", "+", "23" * MaxTtsNumber, "String"),
      StackTestData(".span(_ <= '2')._1", "+", "12" * MaxTtsNumber, "String"),
      StackTestData(".grouped(1).next", "+", "1" * MaxTtsNumber, "String"),
      StackTestData(".partition(_ != '2')._1", "+", "13" * MaxTtsNumber, "String"),
      StackTestData(".partitionMap(Left(_))._1", "+", "123" * MaxTtsNumber, "String"),
      StackTestData(s".lazyZip(Iterable('1', '2', '3')).forall(_ == _)", "&", true, "Boolean"),
      StackTestData(".diff[Char](Seq('2'))", "+", "13" * MaxTtsNumber, "String"),
      StackTestData(".diff[Any](Seq('2'.toInt))", "+", "13" * MaxTtsNumber, "String"),
      StackTestData(".intersect[Char](Seq('3', '4'))", "+", "3" * MaxTtsNumber, "String"),
      StackTestData(".intersect[Any](Seq('3'.toInt, '4'.toInt))", "+", "3" * MaxTtsNumber, "String"),
      StackTestData(".distinct", "+", "123" * MaxTtsNumber, "String"),
      StackTestData(".distinctBy(_.toInt)", "+", "123" * MaxTtsNumber, "String"),
      StackTestData(".sorted[Char]", "+", "123" * MaxTtsNumber, "String"),
      StackTestData(".sortWith(_ > _)", "+", "321" * MaxTtsNumber, "String"),
      StackTestData(".sortBy(-_.toInt)", "+", "321" * MaxTtsNumber, "String"),
      StackTestData(".groupBy(_ - '1')(0)", "+", "1" * MaxTtsNumber, "String"),
      StackTestData(".sliding(1).hasNext", "&", true, "Boolean"),
      StackTestData(".sliding(1, 1).hasNext", "&", true, "Boolean"),
      StackTestData(".combinations(2).length", "+", 3 * MaxTtsNumber, "Int"),
      StackTestData(".permutations.length", "+", 6 * MaxTtsNumber, "Int")
    )

    // --- String extension by implicit conversion to WrappedString via wrapString
    // some methods with the name & params same as StringOps has but return WrappedString are tested with
    // scala.collection.immutable.WrappedString.UnwrapOp.unwrap but it has no tts stack implicits

//    {
//      import sands.sugar.tms.TmsImplicitsCommon._
//      import sands.sugar.tms.replaceByPublicInterfacesDiff
//
//      import scala.annotation.compileTimeOnly
//
//      @replaceByPublicInterfacesDiff @compileTimeOnly(ctom) trait DebugWrappedStringPartialInterface {
//        type TakeMembersOfType = scala.collection.immutable.WrappedString
//        type DropMembersOfType = scala.collection.immutable.StringOps @@ String
//      }
//    }

    implicitExtensionOnTtsStacksTests(options, testValue = "\"123\"", testValueType = "String", typecheckedImplicitMethod = "wrapString", implicitType = "WrappedString",
      // StackTestData(accessor: String, aggregator: String, result: Any, resultType: String, implicitConversionShouldExist: Boolean = true)
      // WrappedString
      StackTestData(".empty.unwrap", "+", "" * MaxTtsNumber, "String"),
      //StringOps slice (from: Int, until: Int): scala.collection.immutable.WrappedString with source signature method slice (from: Int, until: Int): scala.collection.immutable.WrappedString /erasure (from: Int, until: Int): scala.collection.immutable.WrappedString/ of class WrappedString, isSynthetic=false
      //not reachable due to StringOps non-generic overload, does not compile in general without tts StackTestData(".stepper[scala.collection.Stepper[Char]].hasStep", "&", true, "Boolean"),
      StackTestData(".startsWith[Char](Iterator[Char]('1'))", "&", true, "Boolean"),
      StackTestData(".startsWith[Char](Iterator[Char]('1'), 1)", "|", false, "Boolean"),
      StackTestData(".startsWith[Any](Iterator[Any]('1'.toInt, '2'.toInt))", "&", true, "Boolean"),
      StackTestData(".startsWith[Any](Iterator[Any]('1'.toInt, '2'.toInt), 1)", "|", false, "Boolean"),
      StackTestData(".endsWith[Char](Iterable[Char]('3'))", "&", true, "Boolean"),
      StackTestData(".endsWith[Any](Iterable[Any]('2'.toInt, '3'.toInt))", "&", true, "Boolean"),
      StackTestData(".indexOf[Any]('3'.toInt: Any)", "+", 2 * MaxTtsNumber, "Int"),
      StackTestData(".indexOf[Any]('3'.toInt: Any, 0)", "+", 2 * MaxTtsNumber, "Int"),
      StackTestData(".lastIndexOf[Any]('3'.toInt: Any)", "+", 2 * MaxTtsNumber, "Int"),
      StackTestData(".lastIndexOf[Any]('3'.toInt: Any, 0)", "+", -1 * MaxTtsNumber, "Int"),
      //not reachable due to StringOps non-generic overload, does not compile in general without tts StackTestData(".copyToArray[Any](new Array[Any](1), 0, 1).toString", "+", "1" * MaxTtsNumber, "String"),
      StackTestData(".sameElements[Char](Iterator[Char]('1', '2', '3'))", "&", true, "Boolean"),
      StackTestData(".sameElements[Any](Iterator[Any]('1', '2', '3'))", "&", true, "Boolean"),
      // IndexedSeq
      StackTestData(".toIndexedSeq.mkString", "+", "123" * MaxTtsNumber, "String"),
      //Try 2.12 StackTestData(".canEqual(\"12\")", "&", true, "Boolean"),
      StackTestData(".iterableFactory.newBuilder.result.mkString", "+", "" * MaxTtsNumber, "String"),
      // IndexedSeqOps
      //deprecated & doesn't compile even without implicits StackTestData(".view(1, 2).length", "+", 1 * MaxTtsNumber, "Int"),
      //StringOps take (n: Int): scala.collection.immutable.WrappedString with source signature method take (n: Int): C /erasure (n: Int): scala.collection.immutable.WrappedString/ of trait IndexedSeqOps, isSynthetic=false
      //StringOps takeRight (n: Int): scala.collection.immutable.WrappedString with source signature method takeRight (n: Int): C /erasure (n: Int): scala.collection.immutable.WrappedString/ of trait IndexedSeqOps, isSynthetic=false
      //StringOps drop (n: Int): scala.collection.immutable.WrappedString with source signature method drop (n: Int): C /erasure (n: Int): scala.collection.immutable.WrappedString/ of trait IndexedSeqOps, isSynthetic=false
      //StringOps dropRight (n: Int): scala.collection.immutable.WrappedString with source signature method dropRight (n: Int): C /erasure (n: Int): scala.collection.immutable.WrappedString/ of trait IndexedSeqOps, isSynthetic=false
      //StringOps reverse scala.collection.immutable.WrappedString with source signature method reverse C /erasure (): scala.collection.immutable.WrappedString/ of trait IndexedSeqOps, isSynthetic=false
      //doesn't compile even without implicits: StackTestData(".lengthCompare(Iterator())", "+",  1 * MaxTtsNumber, "Int"),
      StackTestData(".search[Char]('2').insertionPoint", "+",  1 * MaxTtsNumber, "Int"),
      StackTestData(".search[Any]('0': Any)(new Ordering[Any] {def compare(x: Any, y: Any) = 0}).insertionPoint", "+", "123".search[Any]('0': Any)(new Ordering[Any] {
        def compare(x: Any, y: Any) = 0
      }).insertionPoint * MaxTtsNumber, "Int"),
      StackTestData(".search[Char]('2', 0, 2).insertionPoint", "+",  1 * MaxTtsNumber, "Int"),
      StackTestData(".search[Any]('0': Any, 0, 2)(new Ordering[Any] {def compare(x: Any, y: Any) = 0}).insertionPoint", "+", "123".search[Any]('0': Any, 0, 2)(new Ordering[Any] {
        def compare(x: Any, y: Any) = 0
      }).insertionPoint * MaxTtsNumber, "Int"),
      // Seq
      StackTestData(".toSeq.mkString", "+", "123" * MaxTtsNumber, "String"),
      // SeqOps
      StackTestData(".union[Char](Seq('4')).mkString", "+", "1234" * MaxTtsNumber, "String"),
      StackTestData(".union[Any](Seq[Any]('4')).mkString", "+", "1234" * MaxTtsNumber, "String"),
      //StringOps distinct scala.collection.immutable.WrappedString with source signature method distinct C /erasure (): scala.collection.immutable.WrappedString/ of trait SeqOps, isSynthetic=false
      //StringOps distinctBy [B](f: Char => B): scala.collection.immutable.WrappedString with source signature method distinctBy [B](f: A => B): C /erasure (f: Function1): scala.collection.immutable.WrappedString/ of trait SeqOps, isSynthetic=false
      StackTestData(".isDefinedAt(4)", "|", false, "Boolean"),
      StackTestData(".segmentLength(_ > '0')", "+", 3 * MaxTtsNumber, "Int"),
      StackTestData(".segmentLength(_ > '0', 1)", "+", 2 * MaxTtsNumber, "Int"),
      StackTestData(".prefixLength(_ <= '2')", "+", 2 * MaxTtsNumber, "Int"),
      //StringOps indexWhere (p: Char => Boolean): Int with source signature method indexWhere (p: A => Boolean): Int /erasure (p: Function1): Int/ of trait SeqOps, isSynthetic=false
      //WrappedString indexOf [B >: Char](elem: B): Int with source signature method indexOf [B >: A](elem: B): Int /erasure (elem: Object): Int/ of trait SeqOps, isSynthetic=false
      //StringOps lastIndexWhere (p: Char => Boolean): Int with source signature method lastIndexWhere (p: A => Boolean): Int /erasure (p: Function1): Int/ of trait SeqOps, isSynthetic=false
      StackTestData(".indexOfSlice[Char](Seq('3'))", "+", 2 * MaxTtsNumber, "Int"),
      StackTestData(".indexOfSlice[Any](Seq('2'.toInt, '3'.toInt))", "+", 1 * MaxTtsNumber, "Int"),
      StackTestData(".indexOfSlice[Char](Seq('3'), 2)", "+", 2 * MaxTtsNumber, "Int"),
      StackTestData(".indexOfSlice[Any](Seq('2'.toInt, '3'.toInt), 1)", "+", 1 * MaxTtsNumber, "Int"),
      StackTestData(".lastIndexOfSlice[Char](Seq('3'))", "+", 2 * MaxTtsNumber, "Int"),
      StackTestData(".lastIndexOfSlice[Any](Seq('2'.toInt, '3'.toInt))", "+", 1 * MaxTtsNumber, "Int"),
      StackTestData(".lastIndexOfSlice[Char](Seq('3'), 2)", "+", 2 * MaxTtsNumber, "Int"),
      StackTestData(".lastIndexOfSlice[Any](Seq('2'.toInt, '3'.toInt), 1)", "+", 1 * MaxTtsNumber, "Int"),
      StackTestData(".findLast(_ >= '2')", "orElse", Some('3'), "Option[Char]"),
      StackTestData(".containsSlice[Char](Seq('2', '3'))", "&", true, "Boolean"),
      StackTestData(".containsSlice[Any](Seq('2'.toInt, '3'.toInt))", "&", true, "Boolean"),
      StackTestData(".contains[Any](50: Any)", "&", true, "Boolean"),
      StackTestData(".reverseMap(identity(_)).mkString", "+", "321" * MaxTtsNumber, "String"),
      //StringOps sorted [B >: Char](implicit ord: Ordering[B]): scala.collection.immutable.WrappedString with source signature method sorted [B >: A](implicit ord: Ordering[B]): C /erasure (ord: scala.math.Ordering): scala.collection.immutable.WrappedString/ of trait SeqOps, isSynthetic=false
      //StringOps sortWith (lt: (Char, Char) => Boolean): scala.collection.immutable.WrappedString with source signature method sortWith (lt: (A, A) => Boolean): C /erasure (lt: Function2): scala.collection.immutable.WrappedString/ of trait SeqOps, isSynthetic=false
      //StringOps sortBy [B](f: Char => B)(implicit ord: Ordering[B]): scala.collection.immutable.WrappedString with source signature method sortBy [B](f: A => B)(implicit ord: Ordering[B]): C /erasure (f: Function1, ord: scala.math.Ordering): scala.collection.immutable.WrappedString/ of trait SeqOps, isSynthetic=false
      //doesn't compile even without implicits: StackTestData(".sizeCompare(Seq())", "+", 1 * MaxTtsNumber, "Int"),
      //StringOps StackTestData(".lengthIs > 0", "&", true, "Boolean"),
      StackTestData(".corresponds[Char](Seq('2', '3', '4'))(_ + 2 == _ + 1)", "&", true, "Boolean"),
      StackTestData(".corresponds[Any](Seq('2'.toInt, '3'.toInt, '4'.toInt))(_ + 1 == _)", "&", true, "Boolean"),
      //StringOps diff [B >: Char](that: scala.collection.Seq[B]): scala.collection.immutable.WrappedString with source signature method diff [B >: A](that: scala.collection.Seq[B]): C /erasure (that: scala.collection.Seq): scala.collection.immutable.WrappedString/ of trait SeqOps, isSynthetic=false
      //StringOps intersect [B >: Char](that: scala.collection.Seq[B]): scala.collection.immutable.WrappedString with source signature method intersect [B >: A](that: scala.collection.Seq[B]): C /erasure (that: scala.collection.Seq): scala.collection.immutable.WrappedString/ of trait SeqOps, isSynthetic=false
      //not reachable due to StringOps non-generic overload, does not compile in general without tts StackTestData(".updated[Char](1, '4').mkString", "+", "1523" * MaxTtsNumber, "String"),
      //not reachable due to StringOps non-generic overload, does not compile in general without tts StackTestData(".updated[Any](1, '4'.toInt: Any).mkString", "+", "1523" * MaxTtsNumber, "String"),
      // PartialFunction
      StackTestData(".unapply(0)", "orElse", Some('1'), "Option[Char]"),
      StackTestData(".elementWise.unapplySeq(Seq(0)).get.mkString", "+", "1" * MaxTtsNumber, "String"),
      //Try   def orElse[A1 <: Int, B1 >: Char](that: PartialFunction[A1,B1]): PartialFunction[A1,B1];
      //Try   def andThen[C](k: Char => C): PartialFunction[Int,C];
      StackTestData(".compose[Int]{case i: Int => i}.apply(0)", "+", '1' * MaxTtsNumber, "Int"),
      StackTestData(".lift(1)", "orElse", Some('2'), "Option[Char]"),
      StackTestData(".applyOrElse(0, (_: Int) => '4')", "+", '1' * MaxTtsNumber, "Int"),
      StackTestData(".runWith(identity(_))(0)", "&", true, "Boolean"),
      // Function1
      // 2.13.5 compiler generates PartialFunction (instead of Function) with wrong code error on macros lifting StackTestData(".compose[Int](identity(_))(0)", "+", '1' * MaxTtsNumber, "Int"),
      StackTestData(".compose[Int](identity _)(0)", "+", '1' * MaxTtsNumber, "Int"),
      // Iterable
      StackTestData(".toIterable.unwrap", "+", "123" * MaxTtsNumber, "String"),
      StackTestData(".seq.unwrap", "+", "123" * MaxTtsNumber, "String"),
      // IterableOps
      StackTestData(".toTraversable.mkString", "+", "123" * MaxTtsNumber, "String"),
      StackTestData(".isTraversableAgain", "&", true, "Boolean"),
      StackTestData(".repr.unwrap", "+", "123" * MaxTtsNumber, "String"),
      StackTestData(".companion.newBuilder[Char].result().mkString", "+", "" * MaxTtsNumber, "String"),
      //StringOps StackTestData(".sizeIs > 0", "&", true, "Boolean"),
      StackTestData(".transpose(_.toString)(0)(0)", "+", '1' * MaxTtsNumber, "Int"),
      //Try filter (pred: Char => Boolean): scala.collection.immutable.WrappedString with source signature method filter (pred: A => Boolean): C /erasure (pred: Function1): scala.collection.immutable.WrappedString/ of trait IterableOps, isSynthetic=false
      //StringOps filterNot (pred: Char => Boolean): scala.collection.immutable.WrappedString with source signature method filterNot (pred: A => Boolean): C /erasure (pred: Function1): scala.collection.immutable.WrappedString/ of trait IterableOps, isSynthetic=false
      //Try      StackTestData(".withFilter(_ == '1').mkString", "+",  "1" * MaxTtsNumber, "String"),
      //StringOps takeWhile (p: Char => Boolean): scala.collection.immutable.WrappedString with source signature method takeWhile (p: A => Boolean): C /erasure (p: Function1): scala.collection.immutable.WrappedString/ of trait IterableOps, isSynthetic=false
      //StringOps dropWhile (p: Char => Boolean): scala.collection.immutable.WrappedString with source signature method dropWhile (p: A => Boolean): C /erasure (p: Function1): scala.collection.immutable.WrappedString/ of trait IterableOps, isSynthetic=false
      //StringOps sliding (size: Int): Iterator[scala.collection.immutable.WrappedString] with source signature method sliding (size: Int): Iterator[C] /erasure (size: Int): Iterator/ of trait IterableOps, isSynthetic=false
      //StringOps tail scala.collection.immutable.WrappedString with source signature method tail C /erasure (): scala.collection.immutable.WrappedString/ of trait IterableOps, isSynthetic=false
      //StringOps init scala.collection.immutable.WrappedString with source signature method init C /erasure (): scala.collection.immutable.WrappedString/ of trait IterableOps, isSynthetic=false
      StackTestData(".groupMap(_ => true)(identity(_)).apply(true).mkString", "+", "123" * MaxTtsNumber, "String"),
      StackTestData(".groupMapReduce(_ => true)(identity(_))((c1, c2) => c1).apply(true).toString", "+", "1" * MaxTtsNumber, "String"),
      StackTestData(".scan[Char]('0')((c1: Char, c2: Char) => (c1 + c2 - '0').toChar).mkString", "+", "0136" * MaxTtsNumber, "String"),
      StackTestData(".scan[Any]('0')((c1: Any, c2: Any) => c1).mkString", "+", "0000" * MaxTtsNumber, "String"),
      StackTestData(".scanLeft('0')((c1: Char, c2: Char) => (c1 + c2 - '0').toChar).mkString", "+", "0136" * MaxTtsNumber, "String"),
      StackTestData(".scanRight('0')((c1: Char, c2: Char) => (c1 + c2 - '0').toChar).mkString", "+", "6530" * MaxTtsNumber, "String"),
      //Try   def flatten[B](implicit asTraversable: Char => scala.collection.GenTraversableOnce[B]): scala.collection.immutable.IndexedSeq[B];
      // is not reachable in general due to StringOps overload StackTestData(".++[Any](Iterator[Any]('4')).mkString", "+", "1234" * MaxTtsNumber, "String"),
      StackTestData(".zip[Char](Iterator('1')).length", "+", 1 * MaxTtsNumber, "Int"),
      StackTestData(".zip[Int](Iterator('1'.toInt)).length", "+", 1 * MaxTtsNumber, "Int"),
      StackTestData(".zipWithIndex.length", "+", 3 * MaxTtsNumber, "Int"),
      StackTestData(".zipAll[Char, Char](Iterable('1', '2'), '0', '3').length", "+", 3 * MaxTtsNumber, "Int"),
      StackTestData(".zipAll[Char, Int](Iterable[Int]('1', '2'), '0', '3'.toInt).length", "+", 3 * MaxTtsNumber, "Int"),
      StackTestData(".unzip(l => (l, l))._1.mkString", "+", "123" * MaxTtsNumber, "String"),
      StackTestData(".unzip3(l => (l, l, l))._1.mkString", "+", "123" * MaxTtsNumber, "String"),
      StackTestData(s".tapEach(_ => '0').mkString", "+", "123" * MaxTtsNumber, "String"),
      // IterableOnceOps
      StackTestData(".hasDefiniteSize", "&", true, "Boolean"),
      StackTestData("./:[String](\"4\")(_ + _.toString)", "+", "4123" * MaxTtsNumber, "String"),
      StackTestData(".:\\[String](\"4\")(_.toString + _)", "+", "1234" * MaxTtsNumber, "String"),
      StackTestData(".reduce[Char]((c1: Char, c2: Char) => (c1 + c2 - '0').toChar)", "+", '6' * MaxTtsNumber, "Int"),
      StackTestData(".reduceOption[Char]((c1: Char, c2: Char) => (c1 + c2 - '0').toChar).nonEmpty", "&", true, "Boolean"),
      StackTestData(".reduceLeft[Char](_ max _)", "+", "123".reduceLeft(_ max _) * MaxTtsNumber, "Int"),
      StackTestData(".reduceRight[Char](_ max _)", "+", "123".reduceRight(_ max _) * MaxTtsNumber, "Int"),
      StackTestData(".reduceLeftOption[Char]((c1: Char, c2: Char) => (c1 + c2 - '0').toChar).nonEmpty", "&", true, "Boolean"),
      StackTestData(".reduceRightOption[Char]((c1: Char, c2: Char) => (c1 + c2 - '0').toChar).nonEmpty", "&", true, "Boolean"),
      StackTestData(".copyToBuffer[Char](scala.collection.mutable.Buffer[Char](3)).toString", "+", "()" * MaxTtsNumber, "String"),
      //not reachable due to StringOps non-generic overload, does not compile in general without tts StackTestData(".copyToArray[Any](Array[Any](3)).toString", "+", "1" * MaxTtsNumber, "String"),
      //not reachable due to StringOps non-generic overload, does not compile in general without tts StackTestData(".copyToArray[Any](Array[Any](2), 1).toString", "+", "0" * MaxTtsNumber, "String"),
      StackTestData(".sum", "+", ('1' + '2' + '3') * MaxTtsNumber, "Int"),
      StackTestData(".product", "+", "123".product.toInt * MaxTtsNumber, "Int"),
      StackTestData(".min", "+", '1' * MaxTtsNumber, "Int"),
      StackTestData(".minOption", "orElse", Some('1'), "Option[Char]"),
      StackTestData(".max", "+", '3' * MaxTtsNumber, "Int"),
      StackTestData(".maxOption", "orElse", Some('3'), "Option[Char]"),
      StackTestData(".maxBy(_.toLong)", "+", '3' * MaxTtsNumber, "Long"),
      StackTestData(".maxByOption(_.toLong)", "orElse", Some('3'), "Option[Char]"),
      StackTestData(".minBy(_.toLong)", "+", '1' * MaxTtsNumber, "Long"),
      StackTestData(".minByOption(_.toLong)", "orElse", Some('1'), "Option[Char]"),
      StackTestData(".collectFirst{case '4' => \"4\"}.isEmpty", "&", true, "Boolean"),
      StackTestData(".aggregate[Int](0)(_ + _ - '0', _ + _)", "+", 6 * MaxTtsNumber, "Int"),
      StackTestData(".corresponds[Char](Iterator('2', '3', '4'))(_ + 2 == _ + 1)", "&", true, "Boolean"),
      StackTestData(".to[Seq[Char]](Seq).mkString", "+", "123" * MaxTtsNumber, "String"),
      StackTestData(".toIterator.hasNext", "&", true, "Boolean"),
      StackTestData(".toList.mkString", "+", "123" * MaxTtsNumber, "String"),
      StackTestData(".toVector.mkString", "+", "123" * MaxTtsNumber, "String"),
      //Pair input toMap [K, V](implicit ev: Char <:< (K, V)): scala.collection.immutable.Map[K,V] with source signature method toMap [K, V](implicit ev: A <:< (K, V)): scala.collection.immutable.Map[K,V] /erasure (ev: <:<): scala.collection.immutable.Map/ of trait IterableOnceOps, isSynthetic=false
      StackTestData(".toSet", "++", "123".toSet, "Set[Char]"),
      StackTestData(".toStream.head", "+", '1' * MaxTtsNumber, "Int"),
      StackTestData(".toBuffer.mkString", "+", "123" * MaxTtsNumber, "String")
    )
  }
}
