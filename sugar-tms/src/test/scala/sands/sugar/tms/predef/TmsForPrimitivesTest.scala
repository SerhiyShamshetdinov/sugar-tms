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
import sands.sugar.tms.TmsTestBase

/*
 * Created by Serhiy Shamshetdinov
 * at 18.02.2021 16:53
 */

class TmsForPrimitivesTest extends TmsTestBase {

  primitivesTests(SimpleForsStackOptions)

  def primitivesTests(tmsOptions: Seq[String]): Unit = {
    val options = literalOptions(tmsOptions)

    behavior of "tms macros on primitives implicit conversions with TmsOptions" + options

    // scala.Byte implicits ---------------------------------------------------

    implicitPrimitivesConversionTest[Byte, Short](options, ".toShort", "1", "1")
    implicitPrimitivesConversionTest[Byte, Int](options, ".toInt", "1", "1")
    implicitPrimitivesConversionTest[Byte, Long](options, ".toLong", "1", "1")
    implicitPrimitivesConversionTest[Byte, Float](options, ".toFloat", "1", "1.0")
    implicitPrimitivesConversionTest[Byte, Double](options, ".toDouble", "1", "1.0")

    // scala.Short implicits ---------------------------------------------------

    implicitPrimitivesConversionTest[Short, Int](options, ".toInt", "1", "1")
    implicitPrimitivesConversionTest[Short, Long](options, ".toLong", "1", "1")
    implicitPrimitivesConversionTest[Short, Float](options, ".toFloat", "1", "1.0")
    implicitPrimitivesConversionTest[Short, Double](options, ".toDouble", "1", "1.0")

    // scala.Char implicits ---------------------------------------------------

    implicitPrimitivesConversionTest[Char, Int](options, ".toInt", "'a'", "97")
    implicitPrimitivesConversionTest[Char, Long](options, ".toLong", "'a'", "97")
    implicitPrimitivesConversionTest[Char, Float](options, ".toFloat", "'a'", "97.0")
    implicitPrimitivesConversionTest[Char, Double](options, ".toDouble", "'a'", "97.0")

    // scala.Int implicits ---------------------------------------------------

    implicitPrimitivesConversionTest[Int, Long](options, ".toLong", "1", "1")
    implicitPrimitivesConversionTest[Int, Float](options, ".toFloat", "1", "1.0")
    implicitPrimitivesConversionTest[Int, Double](options, ".toDouble", "1", "1.0")

    // scala.Long implicits ---------------------------------------------------

    implicitPrimitivesConversionTest[Long, Float](options, ".toFloat", "1", "1.0")
    implicitPrimitivesConversionTest[Long, Double](options, ".toDouble", "1", "1.0")

    // scala.Float implicits ---------------------------------------------------

    implicitPrimitivesConversionTest[Float, Double](options, ".toDouble", "1", "1.0")

    // Scala Predef "Autoboxing" and "Autounboxing" ---------------------------------------------------

    implicitPrimitivesConversionTest[Byte, java.lang.Byte](options, "byte2Byte", "1", "1")
    implicitPrimitivesConversionTest[Short, java.lang.Short](options, "short2Short", "1", "1")
    implicitPrimitivesConversionTest[Char, java.lang.Character](options, "char2Character", "'a'", "a")
    implicitPrimitivesConversionTest[Int, java.lang.Integer](options, "int2Integer", "1", "1")
    implicitPrimitivesConversionTest[Long, java.lang.Long](options, "long2Long", "1", "1")
    implicitPrimitivesConversionTest[Float, java.lang.Float](options, "float2Float", "1", "1.0")
    implicitPrimitivesConversionTest[Double, java.lang.Double](options, "double2Double", "1.0", "1.0")
    implicitPrimitivesConversionTest[Boolean, java.lang.Boolean](options, "boolean2Boolean", "true", "true")

    implicitPrimitivesConversionTest[java.lang.Byte, Byte](options, "Byte2byte", "java.lang.Byte.valueOf(1: Byte)", "1")
    implicitPrimitivesConversionTest[java.lang.Short, Short](options, "Short2short", "java.lang.Short.valueOf(1: Short)", "1")
    implicitPrimitivesConversionTest[java.lang.Character, Char](options, "Character2char", "java.lang.Character.valueOf('a')", "a")
    implicitPrimitivesConversionTest[java.lang.Integer, Int](options, "Integer2int", "java.lang.Integer.valueOf(1: Int)", "1")
    implicitPrimitivesConversionTest[java.lang.Long, Long](options, "Long2long", "java.lang.Long.valueOf(1: Long)", "1")
    implicitPrimitivesConversionTest[java.lang.Float, Float](options, "Float2float", "java.lang.Float.valueOf(1: Float)", "1.0")
    implicitPrimitivesConversionTest[java.lang.Double, Double](options, "Double2double", "java.lang.Double.valueOf(1: Double)", "1.0")
    implicitPrimitivesConversionTest[java.lang.Boolean, Boolean](options, "Boolean2boolean", "java.lang.Boolean.valueOf(false)", "false")

    // Scala Predef LowPriorityImplicits --------------------------------------------------------------

    implicitExtensionOnTtsStacksTests(options, testValue = "-1: Byte", testValueType = "Byte", typecheckedImplicitMethod = "byteWrapper", implicitType = "RichByte",
      // StackTestData(accessor: String, aggregator: String, result: Any, resultType: String, implicitConversionShouldExist: Boolean = true)
      StackTestData(".isValidByte", "&", true, "Boolean")
    )

    implicitExtensionOnTtsStacksTests(options, testValue = "1: Short", testValueType = "Short", typecheckedImplicitMethod = "shortWrapper", implicitType = "RichShort",
      // StackTestData(accessor: String, aggregator: String, result: Any, resultType: String, implicitConversionShouldExist: Boolean = true)
      StackTestData(".isValidShort", "&", true, "Boolean")
    )

    implicitExtensionOnTtsStacksTests(options, testValue = "1", testValueType = "Int", typecheckedImplicitMethod = "intWrapper", implicitType = "RichInt",
      // StackTestData(accessor: String, aggregator: String, result: Any, resultType: String, implicitConversionShouldExist: Boolean = true)
      StackTestData(".signum", "+", MaxTtsNumber, "Int")
    )

    implicitExtensionOnTtsStacksTests(options, testValue = "'a'", testValueType = "Char", typecheckedImplicitMethod = "charWrapper", implicitType = "RichChar",
      // StackTestData(accessor: String, aggregator: String, result: Any, resultType: String, implicitConversionShouldExist: Boolean = true)
      StackTestData(".toUpper", "+", 'A' * MaxTtsNumber, "Int")
    )

    implicitExtensionOnTtsStacksTests(options, testValue = "15L", testValueType = "Long", typecheckedImplicitMethod = "longWrapper", implicitType = "RichLong",
      // StackTestData(accessor: String, aggregator: String, result: Any, resultType: String, implicitConversionShouldExist: Boolean = true)
      StackTestData(".toHexString", "+", "f" * MaxTtsNumber, "String")
    )

    implicitExtensionOnTtsStacksTests(options, testValue = "1.5F", testValueType = "Float", typecheckedImplicitMethod = "floatWrapper", implicitType = "RichFloat",
      // StackTestData(accessor: String, aggregator: String, result: Any, resultType: String, implicitConversionShouldExist: Boolean = true)
      StackTestData(".floor.toString", "+", "1.0" * MaxTtsNumber, "String")
    )

    implicitExtensionOnTtsStacksTests(options, testValue = "1.5D", testValueType = "Double", typecheckedImplicitMethod = "doubleWrapper", implicitType = "RichDouble",
      // StackTestData(accessor: String, aggregator: String, result: Any, resultType: String, implicitConversionShouldExist: Boolean = true)
      StackTestData(".ceil.toString", "+", "2.0" * MaxTtsNumber, "String")
    )

    implicitExtensionOnTtsStacksTests(options, testValue = "false", testValueType = "Boolean", typecheckedImplicitMethod = "booleanWrapper", implicitType = "RichBoolean",
      // StackTestData(accessor: String, aggregator: String, result: Any, resultType: String, implicitConversionShouldExist: Boolean = true)
      StackTestData(".compare(true)", "+", -1 * MaxTtsNumber, "Int")
    )

    it should "get longValue of Int" in new ToolboxTest {
      val testCode: String =
        s"""$ToolboxTestImports
           |
           |tmsFor[Option[Long]]()(Some(1).longValue())
           |
           |""".stripMargin

      evaluateCode(testCode) shouldBe Some(1L)
    }

    it should "get longValue of Integer" in new ToolboxTest {
      val testCode: String =
        s"""$ToolboxTestImports
           |
           |tmsFor[Option[java.lang.Long]]()(Some(Integer.valueOf(1)).longValue())
           |
           |""".stripMargin

      evaluateCode(testCode) shouldBe Some(java.lang.Long.valueOf(1L))
    }

    it should "add Int to Some[Long]" in new ToolboxTest {
      val testCode: String =
        s"""$ToolboxTestImports
           |
           |tmsFor[Option[Long]]$options(1 + Some(2L))
           |
           |""".stripMargin

      evaluateCode(testCode) shouldBe Some(3L)
    }

    it should "add Some[Int] to Long" in new ToolboxTest {
      val testCode: String =
        s"""$ToolboxTestImports
           |
           |tmsFor[Option[Long]]$options(Some(1) + 2L)
           |
           |""".stripMargin

      evaluateCode(testCode) shouldBe Some(3L)
    }

    it should "add Some[Int] to Some[Long]" in new ToolboxTest {
      val testCode: String =
        s"""$ToolboxTestImports
           |
           |tmsFor[Option[Long]]$options(Some(1) + Some(2L))
           |
           |""".stripMargin

      evaluateCode(testCode) shouldBe Some(3L)
    }

    it should "add Long to Some[Int]" in new ToolboxTest {
      val testCode: String =
        s"""$ToolboxTestImports
           |
           |tmsFor[Option[Long]]$options(1L + Some(2))
           |
           |""".stripMargin

      evaluateCode(testCode) shouldBe Some(3L)
    }

    it should "add Some[Long] to Int" in new ToolboxTest {
      val testCode: String =
        s"""$ToolboxTestImports
           |
           |tmsFor[Option[Long]]$options(Some(1L) + 2)
           |
           |""".stripMargin

      evaluateCode(testCode) shouldBe Some(3L)
    }

    it should "add Some[Long] to Some[Int]" in new ToolboxTest {
      val testCode: String =
        s"""$ToolboxTestImports
           |
           |tmsFor[Option[Long]]$options(Some(1L) + Some(2))
           |
           |""".stripMargin

      evaluateCode(testCode) shouldBe Some(3L)
    }

    it should "compare Some[Int] > Long" in new ToolboxTest {
      val testCode: String =
        s"""$ToolboxTestImports
           |
           |tmsFor[Option[Boolean]]$options(Some(1) > 2L)
           |
           |""".stripMargin

      evaluateCode(testCode) shouldBe Some(false)
    }

    it should "compare Some[Long] <= Int" in new ToolboxTest {
      val testCode: String =
        s"""$ToolboxTestImports
           |
           |tmsFor[Option[Boolean]]$options(Some(1L) <= 2)
           |
           |""".stripMargin

      evaluateCode(testCode) shouldBe Some(true)
    }

    it should "compare Some[Long] == Some[Int] with explicit tts1" in new ToolboxTest {
      val testCode: String =
        s"""$ToolboxTestImports
           |
           |tmsFor[Option[Boolean]]$options(tts1(Some(1L)) == tts1(Some(1)))
           |
           |""".stripMargin

      evaluateCode(testCode) shouldBe Some(true)
    }

    it should "apply unary operation to Some[Int]" in new ToolboxTest {
      val testCode: String =
        s"""$ToolboxTestImports
           |
           |tmsFor[Option[Int]]$options(-Some(1))
           |
           |""".stripMargin

      evaluateCode(testCode) shouldBe Some(-1)
    }

    it should "apply unary operation to Some(Boolean)" in new ToolboxTest {
      val testCode: String =
        s"""$ToolboxTestImports
           |
           |tmsFor[Option[Boolean]]$options(!Some(true))
           |
           |""".stripMargin

      evaluateCode(testCode) shouldBe Some(false)
    }

    it should "process tts with primitive type passed where wider type parameter is expected" in new ToolboxTest {
      val testCode: String =
        s"""$ToolboxTestImports
           |
           |tmsFor[Option[Long]]$options(identity[Long](Some(1)))
           |
           |""".stripMargin

      evaluateCode(testCode) shouldBe Some(1L)
    }

    it should "process tts with primitive type accessor passed where wider type parameter is expected" in new ToolboxTest {
      val testCode: String =
        s"""$ToolboxTestImports
           |
           |class TestClassWithInt(val i: Int)
           |
           |tmsFor[Option[Long]]$options( identity[Long]( Some(new TestClassWithInt(3)).i ) )
           |
           |""".stripMargin

      evaluateCode(testCode) shouldBe Some(3L)
    }

    it should "process tts with tagged primitive type parameter where wider type is expected" in new ToolboxTest {
      val testCode: String =
        s"""$ToolboxTestImports
           |
           |tmsFor[Option[Long]]$options(identity[Long](Some(1.asInstanceOf[Int @@ String])))
           |
           |""".stripMargin

      evaluateCode(testCode) shouldBe Some(1L)
    }

    it should "accept operation of tagged primitive type with wider type as parameter" in new ToolboxTest {
      val testCode: String =
        s"""$ToolboxTestImports
           |
           |tmsFor[Option[Long]]$options(Some(1.asInstanceOf[Int @@ String]) - 1L)
           |
           |""".stripMargin

      evaluateCode(testCode) shouldBe Some(0L)
    }

    it should "accept operation of primitive type with wider tagged type as parameter" in new ToolboxTest {
      val testCode: String =
        s"""$ToolboxTestImports
           |
           |tmsFor[Option[Long]]$options(Some(1) - 1L.asInstanceOf[Long @@ String])
           |
           |""".stripMargin

      evaluateCode(testCode) shouldBe Some(0L)
    }

    it should "accept operation on wider type with tagged primitive type as parameter" in new ToolboxTest {
      val testCode: String =
        s"""$ToolboxTestImports
           |
           |tmsFor[Option[Long]]$options(1L - Some(1.asInstanceOf[Int @@ String]))
           |
           |""".stripMargin

      evaluateCode(testCode) shouldBe Some(0L)
    }

    it should "accept operation on wider tagged type with primitive type as parameter" in new ToolboxTest {
      val testCode: String =
        s"""$ToolboxTestImports
           |
           |tmsFor[Option[Long]]$options(1L.asInstanceOf[Long @@ String] - Some(1))
           |
           |""".stripMargin

      evaluateCode(testCode) shouldBe Some(0L)
    }

    it should "accept operation on tts1 wider tagged type with tts1 tagged primitive type as parameter" in new ToolboxTest {
      val testCode: String =
        s"""$ToolboxTestImports
           |
           |tmsFor[Option[Long]]$options(Some(1L.asInstanceOf[Long @@ String]) - Some(1.asInstanceOf[Int @@ String]))
           |
           |""".stripMargin

      evaluateCode(testCode) shouldBe Some(0L)
    }

    it should "accept operation on tts1 tagged type with tts1 wider tagged primitive type as parameter" in new ToolboxTest {
      val testCode: String =
        s"""$ToolboxTestImports
           |
           |tmsFor[Option[Long]]$options(Some(1.asInstanceOf[Int @@ String]) - Some(1L.asInstanceOf[Long @@ String]))
           |
           |""".stripMargin

      evaluateCode(testCode) shouldBe Some(0L)
    }

    it should "accept Rich operation on tagged primitive type" in new ToolboxTest {
      val testCode: String =
        s"""$ToolboxTestImports
           |
           |tmsFor[Option[Int]]$options(Some(1.asInstanceOf[Int @@ String]).signum)
           |
           |""".stripMargin

      evaluateCode(testCode) shouldBe Some(1)
    }

    it should "process tts1 with Int operation on typed None" in new ToolboxTest {
      val testCode: String =
        s"""$ToolboxTestImports
           |
           |tmsFor[Option[Int]]$options(Option.empty[Int] + 1)
           |
           |""".stripMargin

      evaluateCode(testCode) shouldBe None
    }

    it should "process tts1 operation on typed None with wider parameter type" in new ToolboxTest {
      val testCode: String =
        s"""$ToolboxTestImports
           |
           |tmsFor[Option[Long]]$options(Option.empty[Int] + 1L)
           |
           |""".stripMargin

      evaluateCode(testCode) shouldBe None
    }

    it should "process tts1 operation on wider typed None than parameter" in new ToolboxTest {
      val testCode: String =
        s"""$ToolboxTestImports
           |
           |tmsFor[Option[Long]]$options(Option.empty[Long] + 1)
           |
           |""".stripMargin

      evaluateCode(testCode) shouldBe None
    }

    it should "process tts1 with operation with the same typed None as parameter" in new ToolboxTest {
      val testCode: String =
        s"""$ToolboxTestImports
           |
           |tmsFor[Option[Int]]$options(1 + Option.empty[Int])
           |
           |""".stripMargin

      evaluateCode(testCode) shouldBe None
    }

    it should "process tts1 with operation with typed None as parameter of wider type" in new ToolboxTest {
      val testCode: String =
        s"""$ToolboxTestImports
           |
           |tmsFor[Option[Long]]$options(1L + Option.empty[Int])
           |
           |""".stripMargin

      evaluateCode(testCode) shouldBe None
    }

    it should "process tts1 with operation with wider typed None as parameter" in new ToolboxTest {
      val testCode: String =
        s"""$ToolboxTestImports
           |
           |tmsFor[Option[Long]]$options(1 + Option.empty[Long])
           |
           |""".stripMargin

      evaluateCode(testCode) shouldBe None
    }

    it should "process tts1 with operation on typed None with the same typed None as parameter" in new ToolboxTest {
      val testCode: String =
        s"""$ToolboxTestImports
           |
           |tmsFor[Option[Int]]$options(Option.empty[Int] + Option.empty[Int])
           |
           |""".stripMargin

      evaluateCode(testCode) shouldBe None
    }

    it should "process tts1 with operation on typed None with the wider typed None as parameter" in new ToolboxTest {
      val testCode: String =
        s"""$ToolboxTestImports
           |
           |tmsFor[Option[Long]]$options(Option.empty[Int] + Option.empty[Long])
           |
           |""".stripMargin

      evaluateCode(testCode) shouldBe None
    }

    it should "process tts1 with operation on wider typed None than typed None as parameter" in new ToolboxTest {
      val testCode: String =
        s"""$ToolboxTestImports
           |
           |tmsFor[Option[Long]]$options(Option.empty[Long] + Option.empty[Int])
           |
           |""".stripMargin

      evaluateCode(testCode) shouldBe None
    }

    it should "process tts1 with Int operation on typed None with wider typed None as parameter" in new ToolboxTest {
      val testCode: String =
        s"""$ToolboxTestImports
           |
           |tmsFor[Option[Long]]$options(Option.empty[Int] + Option.empty[Long])
           |
           |""".stripMargin

      evaluateCode(testCode) shouldBe None
    }

    it should "process tts1 with typed None passed as parameter" in new ToolboxTest {
      val testCode: String =
        s"""$ToolboxTestImports
           |
           |tmsFor[Option[Int]]$options(identity[Int](Option.empty[Int]))
           |
           |""".stripMargin

      evaluateCode(testCode) shouldBe None
    }

    it should "process tts1 with typed None passed as parameter where wider type expected" in new ToolboxTest {
      val testCode: String =
        s"""$ToolboxTestImports
           |
           |tmsFor[Option[Long]]$options(identity[Long](Option.empty[Int]))
           |
           |""".stripMargin

      evaluateCode(testCode) shouldBe None
    }
  }
}
