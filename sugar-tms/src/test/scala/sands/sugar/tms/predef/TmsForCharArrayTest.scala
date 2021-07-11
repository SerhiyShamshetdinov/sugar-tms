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

import sands.sugar.tms.SemanticVersion
import sands.sugar.tms.TmsForTestOptions._

import scala.util.Success

/*
 * Created by Serhiy Shamshetdinov
 * at 18.02.2021 16:53
 */

class TmsForCharArrayTest extends TmsForArrayTests {

  charArrayTests(SimpleForsStackOptions)

  def charArrayTests(testOptions: Seq[String]): Unit = {
    val options = literalOptions(testOptions)

    behavior of "tms macros Array[Char] native methods & Scala Predef ArrayCharSequence with TmsOptions" + options

    val ArrayCharSequenceStackTests = Seq(
      // StackTestData(accessor: String, aggregator: String, result: Any, resultType: String, implicitConversionShouldExist: Boolean = true)
      StackTestData(".length", "+", 2 * MaxTtsNumber, "Int"), // via implicit to get both accessible
      StackTestData(".length()", "+", 2 * MaxTtsNumber, "Int"), // via implicit to get both accessible
      StackTestData(".charAt(0).toString", "+", "1" * MaxTtsNumber, "String"),
      StackTestData(".subSequence(1, 2).toString", "+", "2" * MaxTtsNumber, "String"),
      StackTestData(".chars.sum", "+", ('1' + '2') * MaxTtsNumber, "Int"),
      StackTestData(".chars().sum", "+", ('1' + '2') * MaxTtsNumber, "Int"),
      StackTestData(".codePoints.sum", "+", ('1' + '2') * MaxTtsNumber, "Int"),
      StackTestData(".codePoints().sum", "+", ('1' + '2') * MaxTtsNumber, "Int")
    ) ++ (if (ScalaStringVersion < "2.13") Seq(
      StackTestData(".__arrayOfChars.mkString", "+", "12" * MaxTtsNumber, "String") // in 2.13: private & other name
    ) else Nil)

    if (SemanticVersion(ScalaStringVersion) < "2.12.13") {
      // --- Array[Char] native methods and extension by implicit conversion to Predef.ArrayCharSequence class
      implicitExtensionOnTtsStacksTests(options, testValue = "Array[Char]('1', '2')", testValueType = "Array[Char]", typecheckedImplicitMethod = "ArrayCharSequence", implicitType = "ArrayCharSequence",
        ArrayCharSequenceStackTests: _*
      )
    } else { // 2.12.13+
      // --- Array[Char] native methods
      implicitExtensionOnTtsStacksTests(options, testValue = "Array[Char]('1', '2')", testValueType = "Array[Char]", typecheckedImplicitMethod = "", implicitType = "",
        // StackTestData(accessor: String, aggregator: String, result: Any, resultType: String, implicitConversionShouldExist: Boolean = true)
        StackTestData(".length", "+", 2 * MaxTtsNumber, "Int") // native only .length without implicits: .length() is not supported since 2.12.13
      )
      // --- Array[Char] extension by explicit conversion to Predef.ArrayCharSequence class
      implicitExtensionOnTtsStacksTests(options, testValue = "ArrayCharSequence(Array[Char]('1', '2'))", testValueType = "Predef.ArrayCharSequence", typecheckedImplicitMethod = "", implicitType = "",
        ArrayCharSequenceStackTests: _*
      )
    }

    it should "access Array[Char] toString accessor via explicit tts1" in new ToolboxTest {
      val testCode: String =
        s"""$ToolboxTestImports
           |
           |val arrayOfChars = Array[Char]('1', '2')
           |
           |tmsFor[Try[String]]$options( tts1(Try(arrayOfChars)).toString )
           |
           |""".stripMargin

      evaluateCode(testCode) should matchPattern {
        case Success(str: String) if str.matches("""\[C@[0-9a-f]{1,8}""") =>
      }
    }

    it should "access Array[Char] toString() method via explicit tts1" in new ToolboxTest {
      val testCode: String =
        s"""$ToolboxTestImports
           |
           |val arrayOfChars = Array[Char]('1', '2')
           |
           |tmsFor[Try[String]]$options( tts1(Try(arrayOfChars)).toString )
           |
           |""".stripMargin

      evaluateCode(testCode) should matchPattern {
        case Success(str: String) if str.matches("""\[C@[0-9a-f]{1,8}""") =>
      }
    }

    arrayTestsOf[Char](testOptions, "Array[Char]('1', '2')", Array[Char]('1', '2'), "'1'", '1')
  }
}


