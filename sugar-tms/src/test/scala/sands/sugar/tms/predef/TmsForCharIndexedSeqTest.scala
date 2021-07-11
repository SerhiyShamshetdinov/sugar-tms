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

import scala.util.Try

/*
 * Created by Serhiy Shamshetdinov
 * at 18.02.2021 16:53
 */

class TmsForCharIndexedSeqTest extends TmsTestBase {

  charIndexedSeqTests(SimpleForsStackOptions)

  def charIndexedSeqTests(tmsOptions: Seq[String]): Unit = {
    val options = literalOptions(tmsOptions)

    behavior of "tms macros IndexedSeq[Char] native methods & Scala Predef SeqCharSequence with TmsOptions" + options

    val seqCharSequenceTestData = Seq(
      // StackTestData(accessor: String, aggregator: String, result: Any, resultType: String, implicitConversionShouldExist: Boolean = true)
      StackTestData(".length", "+", 2 * MaxTtsNumber, "Int"), // via implicit to get both accessible
      StackTestData(".length()", "+", 2 * MaxTtsNumber, "Int"), // via implicit to get both accessible
      StackTestData(".charAt(0).toString", "+", "1" * MaxTtsNumber, "String"),
      StackTestData(".subSequence(1, 2).toString", "+", "2" * MaxTtsNumber, "String"),
      StackTestData(".chars().sum", "+", ('1' + '2') * MaxTtsNumber, "Int"),
      StackTestData(".codePoints().sum", "+", ('1' + '2') * MaxTtsNumber, "Int")
    ) ++ (if (ScalaStringVersion < "2.13") Seq(
      StackTestData(".__sequenceOfChars", "++[Char, IndexedSeq[Char]]", IndexedSeq[Char]('1', '2', '1', '2', '1', '2', '1', '2', '1', '2'), "scala.collection.IndexedSeq[Char]") // in 2.13: private & other name
    ) else Nil)

    if (SemanticVersion(ScalaStringVersion) < "2.12.13") {
      // --- scala.collection.IndexedSeq[Char] extension by implicit conversion to SeqCharSequence class
      implicitExtensionOnTtsStacksTests(options, testValue = "scala.collection.IndexedSeq[Char]('1', '2')", testValueType = "scala.collection.IndexedSeq[Char]", typecheckedImplicitMethod = "SeqCharSequence", implicitType = "SeqCharSequence",
        seqCharSequenceTestData: _*
      )
    } else { // 2.12.13+
      // --- scala.collection.IndexedSeq[Char] .length extensions by implicit conversion to TmsIndexedSeqCharLength class
      implicitExtensionOnTtsStacksTests(options, testValue = "scala.collection.IndexedSeq[Char]('1', '2')", testValueType = "scala.collection.IndexedSeq[Char]", typecheckedImplicitMethod = "", implicitType = "",
        // StackTestData(accessor: String, aggregator: String, result: Any, resultType: String, implicitConversionShouldExist: Boolean = true)
        StackTestData(".length", "+", 2 * MaxTtsNumber, "Int") // native only .length without implicits. .length() is not supported since 2.12.13
      )
      // --- scala.collection.IndexedSeq[Char] extension by explicit conversion to SeqCharSequence class
      implicitExtensionOnTtsStacksTests(options, testValue = "SeqCharSequence(scala.collection.IndexedSeq[Char]('1', '2'))", testValueType = "Predef.SeqCharSequence", typecheckedImplicitMethod = "", implicitType = "",
        seqCharSequenceTestData: _*
      )
    }

    it should "access scala.collection.IndexedSeq[Char] .toString accessor via ttsN" in new ToolboxTest {
      val testCode: String =
        s"""$ToolboxTestImports
           |
           |val indexedSeqOfChars = IndexedSeq[Char]('1', '2')
           |
           |tmsFor[Try[String]]$options( tts1(Try(indexedSeqOfChars)).toString )
           |
           |""".stripMargin

      // result = "Vector(1, 2)": override toString in scala.Predef.SeqCharSequence implicit is not applied to IndexedSeq[Char].toString due to every instance already has toString
      evaluateCode(testCode) shouldBe Try(IndexedSeq[Char]('1', '2').toString)
    }

    it should "access scala.collection.IndexedSeq[Char] .toString() method via ttsN" in new ToolboxTest {
      val testCode: String =
        s"""$ToolboxTestImports
           |
           |val indexedSeqOfChars = IndexedSeq[Char]('1', '2')
           |
           |tmsFor[Try[String]]$options( tts1(Try(indexedSeqOfChars)).toString )
           |
           |""".stripMargin

      // result = "Vector(1, 2)": override toString in scala.Predef.SeqCharSequence implicit is not applied to IndexedSeq[Char].toString due to every instance already has toString
      evaluateCode(testCode) shouldBe Try(IndexedSeq[Char]('1', '2').toString())
    }

    // --- scala.collection.IndexedSeq[Char] native methods

    //    @replaceByPublicInterfacesDiff @compileTimeOnly(ctom) trait DebugIndexedSeqCharPartialInterface {
    //      type TakeMembersOfType = scala.collection.IndexedSeq[Char]
    //      type DropMembersOfType = Object
    //    }

    val indexedSeqChar = IndexedSeq[Char]('1', '2', '3')
    implicitExtensionOnTtsStacksTests(options, testValue = "scala.collection.IndexedSeq[Char]('1', '2', '3')", testValueType = "scala.collection.IndexedSeq[Char]", typecheckedImplicitMethod = "", implicitType = "", (Seq(
      // StackTestData(accessor: String, aggregator: String, result: Any, resultType: String, implicitConversionShouldExist: Boolean = true)
      // IndexedSeq
      StackTestData(".companion.empty.mkString", "+", "" * MaxTtsNumber, "String"),
      StackTestData(".seq.mkString", "+", "123" * MaxTtsNumber, "String"),
      // IndexedSeqLike
      //Try StackTestData(".hashCode()", "+", indexedSeqChar.hashCode() * MaxTtsNumber, "Int"),
      StackTestData(".iterator.next", "+", '1' * MaxTtsNumber, "Int"),
      StackTestData(".toBuffer.mkString", "+", "123" * MaxTtsNumber, "String"),
      // SeqLike
      StackTestData("(0)", "+", '1' * MaxTtsNumber, "Int"),
      StackTestData(".apply(1)", "+", '2' * MaxTtsNumber, "Int"),
      StackTestData(".lengthCompare(3)", "+", 0, "Int"),
      StackTestData(".isEmpty", "|", false, "Boolean"),
      StackTestData(".size", "+", 3 * MaxTtsNumber, "Int"),
      StackTestData(".segmentLength(_ > '0', 1)", "+", 2 * MaxTtsNumber, "Int"),
      StackTestData(".indexWhere(_ > '0', 1)", "+", 1 * MaxTtsNumber, "Int"),
      StackTestData(".lastIndexWhere(_ > '0', 1)", "+", 1 * MaxTtsNumber, "Int"),
      StackTestData(".permutations.length", "+", 6 * MaxTtsNumber, "Int"),
      StackTestData(".combinations(2).length", "+", 3 * MaxTtsNumber, "Int"),
      StackTestData(".reverse.mkString", "+", "321" * MaxTtsNumber, "String"),
      StackTestData(".reverseMap(identity(_)).mkString", "+", "321" * MaxTtsNumber, "String"),
      StackTestData(".reverseIterator.next", "+", '3' * MaxTtsNumber, "Int"),
      StackTestData(s".startsWith($indexedSeqChar, 1)", "|", false, "Boolean"),
      StackTestData(".endsWith(Seq[Char]('3'))", "&", true, "Boolean"),
      StackTestData(".endsWith(Seq[Any]('2'.toInt, '3'.toInt))", "&", true, "Boolean"),
      StackTestData(".indexOfSlice(Seq[Char]('3'))", "+", 2 * MaxTtsNumber, "Int"),
      StackTestData(".indexOfSlice(Seq[Any]('2'.toInt, '3'.toInt))", "+", 1 * MaxTtsNumber, "Int"),
      StackTestData(".indexOfSlice(Seq[Char]('3'), 2)", "+", 2 * MaxTtsNumber, "Int"),
      StackTestData(".indexOfSlice(Seq[Any]('2'.toInt, '3'.toInt), 1)", "+", 1 * MaxTtsNumber, "Int"),
      StackTestData(".lastIndexOfSlice(Seq[Char]('3'))", "+", 2 * MaxTtsNumber, "Int"),
      StackTestData(".lastIndexOfSlice(Seq[Any]('2'.toInt, '3'.toInt))", "+", 1 * MaxTtsNumber, "Int"),
      StackTestData(".lastIndexOfSlice(Seq[Char]('3'), 2)", "+", 2 * MaxTtsNumber, "Int"),
      StackTestData(".lastIndexOfSlice(Seq[Any]('2'.toInt, '3'.toInt), 1)", "+", 1 * MaxTtsNumber, "Int"),
      StackTestData(".containsSlice(Seq[Char]('2', '3'))", "&", true, "Boolean"),
      StackTestData(".containsSlice(Seq[Any]('2'.toInt, '3'.toInt))", "&", true, "Boolean"),
      StackTestData(".contains('3')", "&", true, "Boolean"),
      StackTestData(".contains(50: Any)", "&", true, "Boolean"),
      StackTestData(".union(\"4\").mkString", "+", "1234" * MaxTtsNumber, "String"),
      StackTestData(".diff(\"2\").mkString", "+", "13" * MaxTtsNumber, "String"),
      StackTestData(".intersect(Seq('3', '4')).mkString", "+", "3" * MaxTtsNumber, "String"),
      StackTestData(".intersect(Seq[Any]('3'.toInt, '4'.toInt)).mkString", "+", "3" * MaxTtsNumber, "String"),
      StackTestData(".distinct.mkString", "+", "123" * MaxTtsNumber, "String"),
      StackTestData(".patch(1, Seq('4', '5'), 1).mkString", "+", "1453" * MaxTtsNumber, "String"),
      StackTestData(".patch(1, Seq[Any]('4'.toInt, '5'.toInt), 1).mkString", "+", "152533" * MaxTtsNumber, "String"),
      StackTestData(".updated(1, '4').mkString", "+", "143" * MaxTtsNumber, "String"),
      StackTestData(".updated(1, '4'.toInt: Any).mkString", "+", "1523" * MaxTtsNumber, "String"),
      StackTestData(".+:('4').mkString", "+", "4123" * MaxTtsNumber, "String"),
      StackTestData(".+:('4'.toInt: Any).mkString", "+", "52123" * MaxTtsNumber, "String"),
      StackTestData(".+:(\"4\").mkString", "+", "4123" * MaxTtsNumber, "String"),
      StackTestData(".+:(Seq('4')).mkString.takeRight(6)", "+", "(4)123" * MaxTtsNumber, "String"),
      StackTestData(".+:(Seq[Any]('4'.toInt)).mkString.takeRight(7)", "+", "(52)123" * MaxTtsNumber, "String"),
      StackTestData(".:+('4').mkString", "+", "1234" * MaxTtsNumber, "String"),
      StackTestData(".:+('4'.toInt: Any).mkString", "+", "12352" * MaxTtsNumber, "String"),
      StackTestData(".padTo(4, '4').mkString", "+", "1234" * MaxTtsNumber, "String"),
      StackTestData(".padTo(4, '4'.toInt: Any).mkString", "+", "12352" * MaxTtsNumber, "String"),
      StackTestData(".corresponds(\"234\")(_ + 2 == _ + 1)", "&", true, "Boolean"),
      StackTestData(".corresponds(Seq('2', '3', '4'))(_ + 2 == _ + 1)", "&", true, "Boolean"),
      StackTestData(".corresponds(Seq[Any]('2'.toInt, '3'.toInt, '4'.toInt))(_ + 1 == _)", "&", true, "Boolean"),
      StackTestData(".sortWith(_ > _).mkString", "+", "321" * MaxTtsNumber, "String"),
      StackTestData(".sortBy(-_.toInt).mkString", "+", "321" * MaxTtsNumber, "String"),
      StackTestData(".sorted.mkString", "+", "123" * MaxTtsNumber, "String"),
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
      //Try  foreach()
      StackTestData(".forall(_ > '0')", "&", true, "Boolean"),
      StackTestData(".exists(_ == '1')", "&", true, "Boolean"),
      StackTestData(".find(_ > '2')", "orElse", Some('3'), "Option[Char]"),
      StackTestData(".foldRight(0)(_ + _)", "+", "123".foldRight(0)(_ + _) * MaxTtsNumber, "Int"),
      StackTestData(".reduceRight(_ max _)", "+", "123".reduceRight(_ max _) * MaxTtsNumber, "Int"),
      StackTestData(".toIterable.iterator.hasNext", "&", true, "Boolean"),
      StackTestData(".toIterator.hasNext", "&", true, "Boolean"),
      StackTestData(".head", "+", '1' * MaxTtsNumber, "Int"),
      StackTestData(".slice(1, 2).mkString", "+", "2" * MaxTtsNumber, "String"),
      StackTestData(".take(2).mkString", "+", "12" * MaxTtsNumber, "String"),
      StackTestData(".drop(1).mkString", "+", "23" * MaxTtsNumber, "String"),
      StackTestData(".takeWhile(_ <= '2').mkString", "+", "12" * MaxTtsNumber, "String"),
      StackTestData(".grouped(1).next.mkString", "+", "1" * MaxTtsNumber, "String"),
      StackTestData(".sliding(1).hasNext", "&", true, "Boolean"),
      StackTestData(".sliding(1, 1).hasNext", "&", true, "Boolean"),
      StackTestData(".takeRight(2).mkString", "+", "23" * MaxTtsNumber, "String"),
      StackTestData(".dropRight(2).mkString", "+", "1" * MaxTtsNumber, "String"),
      StackTestData(".copyToArray(new Array[Any](1), 1, 1).toString", "+", (if (ScalaStringVersion < "2.13") "()" else "0") * MaxTtsNumber, "String"),
      StackTestData(".zip(\"0\").length", "+", MaxTtsNumber, "Int"),
      StackTestData(".zipAll(\"12\", '0', '3').length", "+", 3 * MaxTtsNumber, "Int"),
      StackTestData(".zipWithIndex.length", "+", 3 * MaxTtsNumber, "Int"),
      StackTestData(".sameElements(Seq[Int]('1', '2', '3'))", "&", true, "Boolean"),
      StackTestData(".toStream.head", "+", '1' * MaxTtsNumber, "Int"),
      //Try 2.12 StackTestData(".canEqual(\"12\")", "&", true, "Boolean"),
      // GenericTraversableTemplate
      //< 2.13 only, see at the end: StackTestData(".genericBuilder[Char].result().mkString", "+", "" * MaxTtsNumber, "String"),
      StackTestData(".unzip(l => (l, l))._1.mkString", "+", "123" * MaxTtsNumber, "String"),
      StackTestData(".unzip3(l => (l, l, l))._1.mkString", "+", "123" * MaxTtsNumber, "String"),
      //Try method flatten[B](implicit asTraversable: A => scala.collection.GenTraversableOnce[B])CC[B] with signature flatten[B](implicit asTraversable: Char => scala.collection.GenTraversableOnce[B])IndexedSeq[B] /erasure (asTraversable: Function1)IndexedSeq/ of trait GenericTraversableTemplate, isSynthetic=false
      StackTestData(".transpose(_.toString)(0)(0)", "+", '1' * MaxTtsNumber, "Int"),
      // TraversableLike
      StackTestData(".repr.mkString", "+", "123" * MaxTtsNumber, "String"),
      StackTestData(".isTraversableAgain", "&", true, "Boolean"),
      StackTestData(".hasDefiniteSize", "&", true, "Boolean"),
      StackTestData(".++(\"45\").mkString", "+", "12345" * MaxTtsNumber, "String"),
      StackTestData(".++:(\"45\").mkString", "+", "45123" * MaxTtsNumber, "String"),
      StackTestData(".++:(Iterator('1')).last", "+", '3' * MaxTtsNumber, "Int"),
      //Try      StackTestData(".map(_ - '0').sum", "+",  6 * MaxTtsNumber, "Int"),
      //Try      StackTestData(".flatMap(_ => '0')", "+",  "000" * MaxTtsNumber, "String"),
      //Try      StackTestData(".filter(_ => '2')", "+",  "23" * MaxTtsNumber, "String"),
      //Try      StackTestData(".filterNot(_ => '2')", "+",  "1" * MaxTtsNumber, "String"),
      //Try 2.12 StackTestData(".collect{case c if c > '2' => c}.mkString", "+", "3" * MaxTtsNumber, "String"),
      StackTestData(".partition(_ != '2')._1.mkString", "+", "13" * MaxTtsNumber, "String"),
      StackTestData(".groupBy(_ - '1')(0).mkString", "+", "1" * MaxTtsNumber, "String"),
      StackTestData(".scan('0')((c1: Char, c2: Char) => (c1 + c2 - '0').toChar).mkString", "+", "0136" * MaxTtsNumber, "String"),
      StackTestData(".scanLeft('0')((c1: Char, c2: Char) => (c1 + c2 - '0').toChar).mkString", "+", "0136" * MaxTtsNumber, "String"),
      StackTestData(".scanRight('0')((c1: Char, c2: Char) => (c1 + c2 - '0').toChar).mkString", "+", "6530" * MaxTtsNumber, "String"),
      StackTestData(".headOption", "orElse", Some('1'), "Option[Char]"),
      StackTestData(".tail.mkString", "+", "23" * MaxTtsNumber, "String"),
      StackTestData(".last", "+", '3' * MaxTtsNumber, "Int"),
      StackTestData(".lastOption", "orElse", Some('3'), "Option[Char]"),
      StackTestData(".init.mkString", "+", "12" * MaxTtsNumber, "String"),
      StackTestData(".dropWhile(_ <= '2').mkString", "+", "3" * MaxTtsNumber, "String"),
      StackTestData(".span(_ <= '2')._1.mkString", "+", "12" * MaxTtsNumber, "String"),
      StackTestData(".splitAt(1)._2.mkString", "+", "23" * MaxTtsNumber, "String"),
      StackTestData(".tails.next.mkString", "+", "123" * MaxTtsNumber, "String"),
      StackTestData(".inits.next.mkString", "+", "123" * MaxTtsNumber, "String"),
      StackTestData(".toTraversable.mkString", "+", "123" * MaxTtsNumber, "String"),
      //RichChar.to StackTestData(".to[Seq].mkString", "+", "123" * MaxTtsNumber, "String"),
      //< 2.13 only, see at the end: StackTestData(".stringPrefix", "+", "Vector" * MaxTtsNumber, "String"),
      //Try      StackTestData(".withFilter(_ == '1').mkString", "+",  "1" * MaxTtsNumber, "String"),
      // Parallelizable
      //< 2.13 only, see at the end: StackTestData(".par.size", "+", 3 * MaxTtsNumber, "Int"),
      // TraversableOnce
      StackTestData(".nonEmpty", "&", true, "Boolean"),
      StackTestData(".count(_ > '1')", "+", 2 * MaxTtsNumber, "Int"),
      StackTestData(".collectFirst{case '4' => \"4\"}.isEmpty", "&", true, "Boolean"),
      StackTestData("./:(\"4\")(_ + _.toString)", "+", "4123" * MaxTtsNumber, "String"),
      StackTestData(".:\\(\"4\")(_.toString + _)", "+", "1234" * MaxTtsNumber, "String"),
      StackTestData(".foldLeft(0)(_ + _)", "+", "123".foldLeft(0)(_ + _) * MaxTtsNumber, "Int"),
      StackTestData(".reduceLeft(_ max _)", "+", "123".reduceLeft(_ max _) * MaxTtsNumber, "Int"),
      StackTestData(".reduceLeftOption((c1: Char, c2: Char) => (c1 + c2 - '0').toChar).nonEmpty", "&", true, "Boolean"),
      StackTestData(".reduceRightOption((c1: Char, c2: Char) => (c1 + c2 - '0').toChar).nonEmpty", "&", true, "Boolean"),
      StackTestData(".reduce((c1: Char, c2: Char) => (c1 + c2 - '0').toChar)", "+", '6' * MaxTtsNumber, "Int"),
      StackTestData(".reduceOption((c1: Char, c2: Char) => (c1 + c2 - '0').toChar).nonEmpty", "&", true, "Boolean"),
      //Try 2.12 StackTestData(".fold('0')((c1: Char, c2: Char) => (c1 + c2 - '0').toChar)", "+", '6' * MaxTtsNumber, "Int"),
      StackTestData(".aggregate(0)(_ + _ - '0', _ + _)", "+", 6 * MaxTtsNumber, "Int"),
      StackTestData(".sum", "+", ('1' + '2' + '3') * MaxTtsNumber, "Int"),
      StackTestData(".product", "+", "123".product.toInt * MaxTtsNumber, "Int"),
      //RichChar.min StackTestData(".min", "+", '1' * MaxTtsNumber, "Int"),
      //RichChar.max StackTestData(".max", "+", '3' * MaxTtsNumber, "Int"),
      StackTestData(".maxBy(_.toLong)", "+", '3' * MaxTtsNumber, "Long"),
      StackTestData(".minBy(_.toLong)", "+", '1' * MaxTtsNumber, "Long"),
      StackTestData(".copyToBuffer(scala.collection.mutable.Buffer[Char](3)).toString", "+", "()" * MaxTtsNumber, "String"),
      StackTestData(".copyToArray(Array[Char](2), 1).toString", "+", (if (ScalaStringVersion < "2.13") "()" else "0") * MaxTtsNumber, "String"),
      StackTestData(".copyToArray(Array[Char](3)).toString", "+", (if (ScalaStringVersion < "2.13") "()" else "1") * MaxTtsNumber, "String"),
      StackTestData(".toArray[Char].length", "+", 3 * MaxTtsNumber, "Int"),
      StackTestData(".toList.mkString", "+", "123" * MaxTtsNumber, "String"),
      StackTestData(".toIndexedSeq.mkString", "+", "123" * MaxTtsNumber, "String"),
      StackTestData(".toSet", "++", "123".toSet, "Set[Char]"),
      StackTestData(".toVector.mkString", "+", "123" * MaxTtsNumber, "String"),
      StackTestData(""".mkString("<", "-", ">")""", "+", "<1-2-3>" * MaxTtsNumber, "String"),
      StackTestData(""".mkString("-")""", "+", "1-2-3" * MaxTtsNumber, "String"),
      StackTestData(".mkString", "+", "123" * MaxTtsNumber, "String"),
      StackTestData(""".addString(new StringBuilder, "<", "-", ">").toString""", "+", "<1-2-3>" * MaxTtsNumber, "String"),
      StackTestData(""".addString(new StringBuilder, "-").toString""", "+", "1-2-3" * MaxTtsNumber, "String"),
      StackTestData(""".addString(new StringBuilder).toString""", "+", "123" * MaxTtsNumber, "String"),
      // PartialFunction
      //Try   def orElse[A1 <: Int, B1 >: Char](that: PartialFunction[A1,B1]): PartialFunction[A1,B1];
      //Try   def andThen[C](k: Char => C): PartialFunction[Int,C];
      StackTestData(".lift(1)", "orElse", Some('2'), "Option[Char]"),
      StackTestData(".applyOrElse(0, (_: Int) => '4')", "+", '1' * MaxTtsNumber, "Int"),
      StackTestData(".runWith(identity(_))(0)", "&", true, "Boolean"),
      // Function1
      // 2.13.5 compiler generates PartialFunction (instead of Function) with wrong code error on macros lifting StackTestData(".compose[Int](identity(_))(0)", "+", '1' * MaxTtsNumber, "Int")
      StackTestData(".compose[Int](identity _)(0)", "+", '1' * MaxTtsNumber, "Int")
    ) ++ (if (ScalaStringVersion < "2.13") Seq(
      StackTestData(".genericBuilder[Char].result().mkString", "+", "" * MaxTtsNumber, "String"),
      StackTestData(".stringPrefix", "+", "Vector" * MaxTtsNumber, "String"),
      StackTestData(".par.size", "+", 3 * MaxTtsNumber, "Int")
    ) else Nil)): _*
    )
  }
}
