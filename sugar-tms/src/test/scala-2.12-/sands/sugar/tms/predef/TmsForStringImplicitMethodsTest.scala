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

    //    @replaceByPublicInterfacesDiff @compileTimeOnly(ctom) trait DebugWrappedStringPartialInterface {
    //      type TakeMembersOfType = scala.collection.immutable.StringOps
    //      type DropMembersOfType = String
    //    }

    implicitExtensionOnTtsStacksTests(options, testValue = "\"123\"", testValueType = "String", typecheckedImplicitMethod = "augmentString", implicitType = "StringOps",
      // StackTestData(accessor: String, aggregator: String, result: Any, resultType: String, implicitConversionShouldExist: Boolean = true)
      // StringOps
      StackTestData(".repr", "+", "123" * MaxTtsNumber, "String"),
      StackTestData("(0)", "+", '1' * MaxTtsNumber, "Int"),
      StackTestData(".apply(1)", "+", '2' * MaxTtsNumber, "Int"),
      StackTestData(".slice(1, 2)", "+", "2" * MaxTtsNumber, "String"),
      StackTestData(".seq.self", "+", "123" * MaxTtsNumber, "String"),
      // StringLike
      StackTestData(".mkString", "+", "123" * MaxTtsNumber, "String"),
      StackTestData("* 2", "+", "123123" * MaxTtsNumber, "String"),
      StackTestData(".compare(\"123\")", "+", 0, "Int"),
      StackTestData(".stripLineEnd", "+", "123" * MaxTtsNumber, "String"),
      StackTestData(".linesWithSeparators.next()", "+", "123" * MaxTtsNumber, "String"),
      //deprecated StackTestData(".lines.next()", "+", "123" * MaxTtsNumber, "String"),
      StackTestData(".linesIterator.next()", "+", "123" * MaxTtsNumber, "String"),
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
      StackTestData(".toInt", "+", 123 * MaxTtsNumber, "Int"),
      StackTestData(".toArray[Char].length", "+", 3 * MaxTtsNumber, "Int"),
      StackTestData(".format()", "+", "123" * MaxTtsNumber, "String"),
      StackTestData(".formatLocal(java.util.Locale.US)", "+", "123" * MaxTtsNumber, "String"),
      // Ordered
      StackTestData("> \"0\" ", "&", true, "Boolean"),
      StackTestData("<= \"2\" ", "&", true, "Boolean"),
      // IndexedSeqOptimized
      //Try  foreach()
      StackTestData(".forall(_ > '0')", "&", true, "Boolean"),
      StackTestData(".exists(_ == '1')", "&", true, "Boolean"),
      StackTestData(".find(_ > '2')", "orElse", Some('3'), "Option[Char]"),
      StackTestData(".foldLeft(0)(_ + _)", "+", "123".foldLeft(0)(_ + _) * MaxTtsNumber, "Int"),
      StackTestData(".foldRight(0)(_ + _)", "+", "123".foldRight(0)(_ + _) * MaxTtsNumber, "Int"),
      StackTestData(".reduceLeft(_ max _)", "+", "123".reduceLeft(_ max _) * MaxTtsNumber, "Int"),
      StackTestData(".reduceRight(_ max _)", "+", "123".reduceRight(_ max _) * MaxTtsNumber, "Int"),
      StackTestData(".zip(\"0\").length", "+", MaxTtsNumber, "Int"),
      StackTestData(".zipWithIndex.length", "+", 3 * MaxTtsNumber, "Int"),
      StackTestData(".head", "+", '1' * MaxTtsNumber, "Int"),
      StackTestData(".tail", "+", "23" * MaxTtsNumber, "String"),
      StackTestData(".last", "+", '3' * MaxTtsNumber, "Int"),
      StackTestData(".init", "+", "12" * MaxTtsNumber, "String"),
      StackTestData(".take(2)", "+", "12" * MaxTtsNumber, "String"),
      StackTestData(".drop(1)", "+", "23" * MaxTtsNumber, "String"),
      StackTestData(".takeRight(2)", "+", "23" * MaxTtsNumber, "String"),
      StackTestData(".dropRight(2)", "+", "1" * MaxTtsNumber, "String"),
      StackTestData(".splitAt(1)._2", "+", "23" * MaxTtsNumber, "String"),
      StackTestData(".takeWhile(_ <= '2')", "+", "12" * MaxTtsNumber, "String"),
      StackTestData(".dropWhile(_ <= '2')", "+", "3" * MaxTtsNumber, "String"),
      StackTestData(".span(_ <= '2')._1", "+", "12" * MaxTtsNumber, "String"),
      StackTestData(".sameElements(Seq[Int]('1', '2', '3'))", "&", true, "Boolean"),
      StackTestData(".copyToArray(new Array[Any](1), 1, 1).toString", "+", "()" * MaxTtsNumber, "String"),
      StackTestData(".lengthCompare(3)", "+", 0, "Int"),
      StackTestData(".segmentLength(_ > '0', 1)", "+", 2 * MaxTtsNumber, "Int"),
      StackTestData(".indexWhere(_ > '0', 1)", "+", 1 * MaxTtsNumber, "Int"),
      StackTestData(".lastIndexWhere(_ > '0', 1)", "+", 1 * MaxTtsNumber, "Int"),
      StackTestData(".reverse", "+", "321" * MaxTtsNumber, "String"),
      StackTestData(".reverseIterator.next", "+", '3' * MaxTtsNumber, "Int"),
      StackTestData(".endsWith(Seq[Char]('3'))", "&", true, "Boolean"),
      StackTestData(".endsWith(Seq[Any]('2'.toInt, '3'.toInt))", "&", true, "Boolean"),
      // IndexedSeqLike
      StackTestData(".iterator.next", "+", '1' * MaxTtsNumber, "Int"),
      StackTestData(".toBuffer.mkString", "+", "123" * MaxTtsNumber, "String"),
      // SeqLike
      StackTestData(".size", "+", 3 * MaxTtsNumber, "Int"),
      StackTestData(".permutations.length", "+", 6 * MaxTtsNumber, "Int"),
      StackTestData(".combinations(2).length", "+", 3 * MaxTtsNumber, "Int"),
      StackTestData(".reverseMap(identity(_))", "+", "321" * MaxTtsNumber, "String"),
      StackTestData(".indexOfSlice(\"3\")", "+", 2 * MaxTtsNumber, "Int"),
      StackTestData(".indexOfSlice(Seq[Char]('3'))", "+", 2 * MaxTtsNumber, "Int"),
      StackTestData(".indexOfSlice(Seq[Any]('2'.toInt, '3'.toInt))", "+", 1 * MaxTtsNumber, "Int"),
      StackTestData(".indexOfSlice(\"3\", 2)", "+", 2 * MaxTtsNumber, "Int"),
      StackTestData(".indexOfSlice(Seq[Char]('3'), 2)", "+", 2 * MaxTtsNumber, "Int"),
      StackTestData(".indexOfSlice(Seq[Any]('2'.toInt, '3'.toInt), 1)", "+", 1 * MaxTtsNumber, "Int"),
      StackTestData(".lastIndexOfSlice(\"3\")", "+", 2 * MaxTtsNumber, "Int"),
      StackTestData(".lastIndexOfSlice(Seq[Char]('3'))", "+", 2 * MaxTtsNumber, "Int"),
      StackTestData(".lastIndexOfSlice(Seq[Any]('2'.toInt, '3'.toInt))", "+", 1 * MaxTtsNumber, "Int"),
      StackTestData(".lastIndexOfSlice(\"3\", 2)", "+", 2 * MaxTtsNumber, "Int"),
      StackTestData(".lastIndexOfSlice(Seq[Char]('3'), 2)", "+", 2 * MaxTtsNumber, "Int"),
      StackTestData(".lastIndexOfSlice(Seq[Any]('2'.toInt, '3'.toInt), 1)", "+", 1 * MaxTtsNumber, "Int"),
      StackTestData(".containsSlice(\"23\")", "&", true, "Boolean"),
      StackTestData(".containsSlice(Seq[Char]('2', '3'))", "&", true, "Boolean"),
      StackTestData(".containsSlice(Seq[Any]('2'.toInt, '3'.toInt))", "&", true, "Boolean"),
      StackTestData(".contains('3')", "&", true, "Boolean"),
      StackTestData(".contains[Any](50: Any)", "&", true, "Boolean"),
      StackTestData(".union(\"4\")", "+", "1234" * MaxTtsNumber, "String"),
      StackTestData(".diff(\"2\")", "+", "13" * MaxTtsNumber, "String"),
      StackTestData(".intersect(\"34\")", "+", "3" * MaxTtsNumber, "String"),
      StackTestData(".intersect(Seq('3', '4'))", "+", "3" * MaxTtsNumber, "String"),
      StackTestData(".intersect(Seq[Any]('3'.toInt, '4'.toInt))", "+", "3" * MaxTtsNumber, "String"),
      StackTestData(".distinct", "+", "123" * MaxTtsNumber, "String"),
      StackTestData(".patch(1, \"45\", 1)", "+", "1453" * MaxTtsNumber, "String"),
      StackTestData(".patch(1, Seq('4', '5'), 1)", "+", "1453" * MaxTtsNumber, "String"),
      StackTestData(".patch(1, Seq[Any]('4'.toInt, '5'.toInt), 1).mkString", "+", "152533" * MaxTtsNumber, "String"),
      StackTestData(".updated(1, '4')", "+", "143" * MaxTtsNumber, "String"),
      StackTestData(".updated(1, '4'.toInt: Any).mkString", "+", "1523" * MaxTtsNumber, "String"),
      StackTestData(".+:('4').mkString", "+", "4123" * MaxTtsNumber, "String"),
      StackTestData(".+:('4'.toInt: Any).mkString", "+", "52123" * MaxTtsNumber, "String"),
      StackTestData(".+:(\"4\").mkString", "+", "4123" * MaxTtsNumber, "String"),
      StackTestData(".+:(Seq('4')).mkString.takeRight(6)", "+", "(4)123" * MaxTtsNumber, "String"),
      StackTestData(".+:(Seq[Any]('4'.toInt)).mkString.takeRight(7)", "+", "(52)123" * MaxTtsNumber, "String"),
      StackTestData(".:+('4')", "+", "1234" * MaxTtsNumber, "String"),
      StackTestData(".:+('4'.toInt: Any).mkString", "+", "12352" * MaxTtsNumber, "String"),
      StackTestData(".padTo(4, '4')", "+", "1234" * MaxTtsNumber, "String"),
      StackTestData(".padTo(4, '4'.toInt: Any).mkString", "+", "12352" * MaxTtsNumber, "String"),
      StackTestData(".corresponds(\"234\")(_ + 2 == _ + 1)", "&", true, "Boolean"),
      StackTestData(".corresponds(Seq('2', '3', '4'))(_ + 2 == _ + 1)", "&", true, "Boolean"),
      StackTestData(".corresponds(Seq[Any]('2'.toInt, '3'.toInt, '4'.toInt))(_ + 1 == _)", "&", true, "Boolean"),
      StackTestData(".sortWith(_ > _)", "+", "321" * MaxTtsNumber, "String"),
      StackTestData(".sortBy(-_.toInt)", "+", "321" * MaxTtsNumber, "String"),
      StackTestData(".sorted", "+", "123" * MaxTtsNumber, "String"),
      StackTestData(".toSeq.mkString", "+", "123" * MaxTtsNumber, "String"),
      StackTestData(".indices.last", "+", 2 * MaxTtsNumber, "Int"),
      StackTestData(".view.length", "+", 3 * MaxTtsNumber, "Int"),
      StackTestData(".view(1, 2).length", "+", 1 * MaxTtsNumber, "Int"),
      // GenSeqLike
      StackTestData(".isDefinedAt(4)", "|", false, "Boolean"),
      StackTestData(".prefixLength(_ <= '2')", "+", 2 * MaxTtsNumber, "Int"),
      StackTestData(".indexWhere(_ >= '2')", "+", 1 * MaxTtsNumber, "Int"),
      StackTestData(".indexOf('3'.toInt: Any)", "+", 2 * MaxTtsNumber, "Int"),
      StackTestData(".indexOf('3'.toInt: Any, 0)", "+", 2 * MaxTtsNumber, "Int"),
      StackTestData(".lastIndexOf('3'.toInt: Any)", "+", 2 * MaxTtsNumber, "Int"),
      StackTestData(".lastIndexOf('3'.toInt: Any, 0)", "+", -1 * MaxTtsNumber, "Int"),
      StackTestData(".lastIndexWhere(_ >= '2')", "+", 2 * MaxTtsNumber, "Int"),
      StackTestData(".startsWith(Seq[Char]('1'))", "&", true, "Boolean"),
      StackTestData(".startsWith(Seq[Any]('1'.toInt, '2'.toInt))", "&", true, "Boolean"),
      // IterableLike
      StackTestData(".toIterable.iterator.hasNext", "&", true, "Boolean"),
      StackTestData(".toIterator.hasNext", "&", true, "Boolean"),
      StackTestData(".grouped(1).next", "+", "1" * MaxTtsNumber, "String"),
      StackTestData(".sliding(1).hasNext", "&", true, "Boolean"),
      StackTestData(".sliding(1, 1).hasNext", "&", true, "Boolean"),
      StackTestData(".zipAll(\"12\", '0', '3').length", "+", 3 * MaxTtsNumber, "Int"),
      StackTestData(".toStream.head", "+", '1' * MaxTtsNumber, "Int"),
      //Try 2.12 StackTestData(".canEqual(\"12\")", "&", true, "Boolean"),
      // TraversableLike
      StackTestData(".isTraversableAgain", "&", true, "Boolean"),
      StackTestData(".hasDefiniteSize", "&", true, "Boolean"),
      StackTestData(".++(\"45\")", "+", "12345" * MaxTtsNumber, "String"),
      StackTestData(".++:(\"45\")", "+", "45123" * MaxTtsNumber, "String"),
      StackTestData(".++:(Iterator('1')).last", "+", '3' * MaxTtsNumber, "Int"),
      //Try      StackTestData(".map(_ - '0').sum", "+",  6 * MaxTtsNumber, "Int"),
      //Try      StackTestData(".flatMap(_ => '0')", "+",  "000" * MaxTtsNumber, "String"),
      //Try      StackTestData(".filter(_ => '2')", "+",  "23" * MaxTtsNumber, "String"),
      //Try      StackTestData(".filterNot(_ => '2')", "+",  "1" * MaxTtsNumber, "String"),
      //Try 2.12 StackTestData(".collect{case c if c > '2' => c}", "+", "3" * MaxTtsNumber, "String"),
      StackTestData(".partition(_ != '2')._1", "+", "13" * MaxTtsNumber, "String"),
      StackTestData(".groupBy(_ - '1')(0)", "+", "1" * MaxTtsNumber, "String"),
      StackTestData(".scan('0')((c1: Char, c2: Char) => (c1 + c2 - '0').toChar)", "+", "0136" * MaxTtsNumber, "String"),
      StackTestData(".scanLeft('0')((c1: Char, c2: Char) => (c1 + c2 - '0').toChar)", "+", "0136" * MaxTtsNumber, "String"),
      StackTestData(".scanRight('0')((c1: Char, c2: Char) => (c1 + c2 - '0').toChar)", "+", "6530" * MaxTtsNumber, "String"),
      StackTestData(".headOption", "orElse", Some('1'), "Option[Char]"),
      StackTestData(".lastOption", "orElse", Some('3'), "Option[Char]"),
      StackTestData(".tails.next", "+", "123" * MaxTtsNumber, "String"),
      StackTestData(".inits.next", "+", "123" * MaxTtsNumber, "String"),
      StackTestData(".toTraversable.mkString", "+", "123" * MaxTtsNumber, "String"),
      StackTestData(".to[Seq].mkString", "+", "123" * MaxTtsNumber, "String"),
      StackTestData(".stringPrefix", "+", "String" * MaxTtsNumber, "String"),
      //Try      StackTestData(".withFilter(_ == '1').mkString", "+",  "1" * MaxTtsNumber, "String"),
      // Parallelizable
      StackTestData(".par.size", "+", 3 * MaxTtsNumber, "Int"),
      // TraversableOnce
      StackTestData(".nonEmpty", "&", true, "Boolean"),
      StackTestData(".collectFirst{case '4' => \"4\"}.isEmpty", "&", true, "Boolean"),
      StackTestData(".count(_ > '1')", "+", 2 * MaxTtsNumber, "Int"),
      StackTestData("./:(\"4\")(_ + _.toString)", "+", "4123" * MaxTtsNumber, "String"),
      StackTestData(".:\\(\"4\")(_.toString + _)", "+", "1234" * MaxTtsNumber, "String"),
      StackTestData(".reduceLeftOption((c1: Char, c2: Char) => (c1 + c2 - '0').toChar).nonEmpty", "&", true, "Boolean"),
      StackTestData(".reduceRightOption((c1: Char, c2: Char) => (c1 + c2 - '0').toChar).nonEmpty", "&", true, "Boolean"),
      StackTestData(".reduce((c1: Char, c2: Char) => (c1 + c2 - '0').toChar)", "+", '6' * MaxTtsNumber, "Int"),
      StackTestData(".reduceOption((c1: Char, c2: Char) => (c1 + c2 - '0').toChar).nonEmpty", "&", true, "Boolean"),
      //Try 2.12 StackTestData(".fold('0')((c1: Char, c2: Char) => (c1 + c2 - '0').toChar)", "+", '6' * MaxTtsNumber, "Int"),
      StackTestData(".aggregate(0)(_ + _ - '0', _ + _)", "+", 6 * MaxTtsNumber, "Int"),
      StackTestData(".sum", "+", ('1' + '2' + '3') * MaxTtsNumber, "Int"),
      StackTestData(".product", "+", "123".product.toInt * MaxTtsNumber, "Int"),
      StackTestData(".min", "+", '1' * MaxTtsNumber, "Int"),
      StackTestData(".max", "+", '3' * MaxTtsNumber, "Int"),
      StackTestData(".maxBy(_.toLong)", "+", '3' * MaxTtsNumber, "Long"),
      StackTestData(".minBy(_.toLong)", "+", '1' * MaxTtsNumber, "Long"),
      StackTestData(".copyToBuffer(scala.collection.mutable.Buffer[Char](3)).toString", "+", "()" * MaxTtsNumber, "String"),
      StackTestData(".copyToArray(Array[Char](2), 1).toString", "+", "()" * MaxTtsNumber, "String"),
      StackTestData(".copyToArray(Array[Char](3)).toString", "+", "()" * MaxTtsNumber, "String"),
      StackTestData(".toList.mkString", "+", "123" * MaxTtsNumber, "String"),
      StackTestData(".toIndexedSeq.mkString", "+", "123" * MaxTtsNumber, "String"),
      StackTestData(".toSet", "++", "123".toSet, "Set[Char]"),
      StackTestData(".toVector.mkString", "+", "123" * MaxTtsNumber, "String"),
      StackTestData(""".mkString("<", "-", ">")""", "+", "<1-2-3>" * MaxTtsNumber, "String"),
      StackTestData(""".mkString("-")""", "+", "1-2-3" * MaxTtsNumber, "String"),
      StackTestData(""".addString(new StringBuilder, "<", "-", ">").toString""", "+", "<1-2-3>" * MaxTtsNumber, "String"),
      StackTestData(""".addString(new StringBuilder, "-").toString""", "+", "1-2-3" * MaxTtsNumber, "String"),
      StackTestData(""".addString(new StringBuilder).toString""", "+", "123" * MaxTtsNumber, "String")
    )

    implicitExtensionOnTtsStacksTests(options, testValue = "new scala.collection.immutable.StringOps(\"1\")", testValueType = "StringOps", typecheckedImplicitMethod = "unaugmentString", implicitType = "String",
      // StackTestData(accessor: String, aggregator: String, result: Any, resultType: String, implicitConversionShouldExist: Boolean = true)
      StackTestData(".concat(\" \")", "+", "1 " * MaxTtsNumber, "String")
    )

    // --- String extension by implicit conversion to WrappedString via wrapString

    //    @replaceByPublicInterfacesDiff @compileTimeOnly(ctom) trait DebugWrappedStringPartialInterface {
    //      type TakeMembersOfType = scala.collection.immutable.WrappedString
    //      type DropMembersOfType = scala.collection.immutable.StringOps @@ String
    //    }

    implicitExtensionOnTtsStacksTests(options, testValue = "\"123\"", testValueType = "String", typecheckedImplicitMethod = "wrapString", implicitType = "WrappedString",
      // StackTestData(accessor: String, aggregator: String, result: Any, resultType: String, implicitConversionShouldExist: Boolean = true)
      StackTestData(".self", "+", "123" * MaxTtsNumber, "String"),
      StackTestData(".companion.newBuilder[Char].result().mkString", "+", "" * MaxTtsNumber, "String"),
      //Try   def orElse[A1 <: Int, B1 >: Char](that: PartialFunction[A1,B1]): PartialFunction[A1,B1];
      //Try   def andThen[C](k: Char => C): PartialFunction[Int,C];
      StackTestData(".lift(1)", "orElse", Some('2'), "Option[Char]"),
      StackTestData(".applyOrElse(0, (_: Int) => '4')", "+", '1' * MaxTtsNumber, "Int"),
      StackTestData(".runWith(identity(_))(0)", "&", true, "Boolean"),
      StackTestData(".compose[Int](identity(_))(0)", "+", '1' * MaxTtsNumber, "Int"),
      StackTestData(".genericBuilder[Char].result().mkString", "+", "" * MaxTtsNumber, "String"),
      StackTestData(".unzip(l => (l, l))._1.mkString", "+", "123" * MaxTtsNumber, "String"),
      StackTestData(".unzip3(l => (l, l, l))._1.mkString", "+", "123" * MaxTtsNumber, "String"),
      //Try   def flatten[B](implicit asTraversable: Char => scala.collection.GenTraversableOnce[B]): scala.collection.immutable.IndexedSeq[B];
      StackTestData(".transpose(_.toString)(0)(0)", "+", '1' * MaxTtsNumber, "Int")
      //StringOps  StackTestData(".repr.self", "+", "123" * MaxTtsNumber, "String")
    )

    implicitExtensionOnTtsStacksTests(options, testValue = "new scala.collection.immutable.WrappedString(\"1\")", testValueType = "String", typecheckedImplicitMethod = "unwrapString", implicitType = "String",
      // StackTestData(accessor: String, aggregator: String, result: Any, resultType: String, implicitConversionShouldExist: Boolean = true)
      StackTestData(".concat(\" \")", "+", "1 " * MaxTtsNumber, "String")
    )
  }
}
