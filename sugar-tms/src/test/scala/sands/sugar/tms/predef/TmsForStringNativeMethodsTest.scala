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

/*
 * Created by Serhiy Shamshetdinov
 * at 18.02.2021 16:53
 */

class TmsForStringNativeMethodsTest extends TmsTestBase {

  stringNativeMethodsTests(SimpleForsStackOptions)

  def stringNativeMethodsTests(tmsOptions: Seq[String]): Unit = {
    val options = literalOptions(tmsOptions)

    behavior of "tms macros Scala Predef String native methods implicits with TmsOptions" + options

    // --- String native methods

//      import sands.sugar.tms.TmsImplicitsCommon.ctom
//      import sands.sugar.tms._
//      import scala.annotation.compileTimeOnly
//
//      @replaceByPublicInterfacesDiff @compileTimeOnly(ctom) trait DebugStringPartialInterface {
//        type TakeMembersOfType = String
//        type DropMembersOfType = Object
//      }

    val testString = " 123 "
    val testValue = s""""${testString}""""

    val StackTestsJDK8 = Seq(
      // StackTestData(accessor: String, aggregator: String, result: Any, resultType: String, implicitConversionShouldExist: Boolean = true)
      StackTestData(".length", "+", 5 * MaxTtsNumber, "Int"),
      StackTestData(".length()", "+", 5 * MaxTtsNumber, "Int"),
      StackTestData(".isEmpty", "|", false, "Boolean"),
      StackTestData(".isEmpty()", "|", false, "Boolean"),
      StackTestData(".charAt(2)", "+", '2' * MaxTtsNumber, "Int"),
      StackTestData(".codePointAt(2)", "+", '2' * MaxTtsNumber, "Int"),
      StackTestData(".codePointBefore(2)", "+", '1' * MaxTtsNumber, "Int"),
      StackTestData(".codePointCount(0, 2)", "+", 2 * MaxTtsNumber, "Int"),
      StackTestData(".offsetByCodePoints(0, 2)", "+", 2 * MaxTtsNumber, "Int"),
      StackTestData(".getChars(0, 1, new Array[Char](1), 0).toString", "+", "()" * MaxTtsNumber, "String"),
      // deprecated StackTestData(".getBytes(0, 1, new Array[Byte](2), 0).toString", "+", "()" * MaxTtsNumber, "String"),
      StackTestData(".getBytes(\"UTF-8\")(2)", "+", '2' * MaxTtsNumber, "Int"),
      StackTestData(".getBytes(java.nio.charset.StandardCharsets.UTF_8)(3)", "+", '3' * MaxTtsNumber, "Int"),
      StackTestData(".getBytes()(0)", "+", ' ' * MaxTtsNumber, "Int"),
      StackTestData(s".contentEquals(new java.lang.StringBuffer($testValue))", "&", true, "Boolean"),
      StackTestData(".contentEquals(\"0\")", "|", false, "Boolean"),
      StackTestData(".equalsIgnoreCase(\"0\")", "|", false, "Boolean"),
      StackTestData(s".compareTo($testValue)", "+", 0, "Int"),
      StackTestData(s".compareToIgnoreCase($testValue)", "+", 0, "Int"),
      StackTestData(s".regionMatches(2, $testValue, 2, 1)", "&", true, "Boolean"),
      StackTestData(s".regionMatches(true, 2, $testValue, 2, 1)", "&", true, "Boolean"),
      StackTestData(s".startsWith($testValue, 1)", "|", false, "Boolean"),
      StackTestData(s".startsWith($testValue)", "&", true, "Boolean"),
      StackTestData(s".endsWith($testValue)", "&", true, "Boolean"),
      StackTestData(".indexOf('2'.toInt)", "+", 2 * MaxTtsNumber, "Int"),
      StackTestData(".indexOf('2'.toInt, 0)", "+", 2 * MaxTtsNumber, "Int"),
      StackTestData(".lastIndexOf('2'.toInt)", "+", 2 * MaxTtsNumber, "Int"),
      StackTestData(".lastIndexOf('2'.toInt, 1)", "+", -1 * MaxTtsNumber, "Int"),
      StackTestData(".indexOf(\"2\")", "+", 2 * MaxTtsNumber, "Int"),
      StackTestData(".indexOf(\"2\", 0)", "+", 2 * MaxTtsNumber, "Int"),
      StackTestData(".lastIndexOf(\"2\")", "+", 2 * MaxTtsNumber, "Int"),
      StackTestData(".lastIndexOf(\"2\", 1)", "+", -1 * MaxTtsNumber, "Int"),
      StackTestData(".substring(1)", "+", testString.tail * MaxTtsNumber, "String"),
      StackTestData(".substring(1, 2)", "+", testString(1).toString * MaxTtsNumber, "String"),
      StackTestData(".subSequence(1, 2).toString", "+", testString(1).toString * MaxTtsNumber, "String"),
      StackTestData(".concat(\"0\")", "+", (testString + "0") * MaxTtsNumber, "String"),
      StackTestData(".replace(' ', '0')", "+", testString.replaceAll(" ", "0") * MaxTtsNumber, "String"),
      StackTestData(s""".matches("${testString.toSet.mkString("[", "", "]*")}")""", "&", true, "Boolean"),
      StackTestData(s""".contains("${testString.tail}")""", "&", true, "Boolean"),
      StackTestData(s""".replaceFirst("${testString(0)}", "0")""", "+", testString.replaceFirst(" ", "0") * MaxTtsNumber, "String"),
      StackTestData(s""".replaceAll("${testString(0)}", "0")""", "+", testString.replaceAll(" ", "0") * MaxTtsNumber, "String"),
      StackTestData(s""".replace($testValue, "")""", "+", "", "String"),
      StackTestData(s""".split("${testString(2)}", 1).length""", "+", 1 * MaxTtsNumber, "Int"),
      StackTestData(s""".split("${testString(2)}").length""", "+", 2 * MaxTtsNumber, "Int"),
      StackTestData(".toLowerCase(java.util.Locale.US)", "+", testString.toLowerCase(java.util.Locale.US) * MaxTtsNumber, "String"),
      StackTestData(".toLowerCase()", "+", testString.toLowerCase * MaxTtsNumber, "String"),
      StackTestData(".toUpperCase(java.util.Locale.US)", "+", testString.toUpperCase(java.util.Locale.US) * MaxTtsNumber, "String"),
      StackTestData(".toUpperCase()", "+", testString.toUpperCase * MaxTtsNumber, "String"),
      StackTestData(".trim()", "+", testString.trim * MaxTtsNumber, "String"),
      StackTestData(".toCharArray().length", "+", testString.length * MaxTtsNumber, "Int"),
      StackTestData(".intern()", "+", testString * MaxTtsNumber, "String"),
      StackTestData(".$plus(1)", "+", (testString + 1) * MaxTtsNumber, "String"),
      StackTestData(".chars().sum", "+", testString.sum * MaxTtsNumber, "Int"),
      StackTestData(".codePoints().sum", "+", testString.sum * MaxTtsNumber, "Int")
    )

    val StackTestsJDK11 = Seq(
      // StackTestData(accessor: String, aggregator: String, result: Any, resultType: String, implicitConversionShouldExist: Boolean = true)
      StackTestData(".isBlank()", "|", false, "Boolean"),
      StackTestData(".lines().count", "+", 1 * MaxTtsNumber, "Long"),
      StackTestData(".repeat(2)", "+", testString * 2 * MaxTtsNumber, "String"),
      StackTestData(".strip()", "+", testString.trim * MaxTtsNumber, "String"),
      StackTestData(".stripLeading()", "+", testString.tail * MaxTtsNumber, "String"),
      StackTestData(".stripTrailing()", "+", testString.init * MaxTtsNumber, "String")
    )

    val StackTestsJDK12 = Seq(
      // StackTestData(accessor: String, aggregator: String, result: Any, resultType: String, implicitConversionShouldExist: Boolean = true)
      StackTestData(".indent(-1).length", "+", testString.length * MaxTtsNumber, "Int"),
      //Try StackTestData(".transform(_.length)", "+", testString.length * MaxTtsNumber, "Int"),
      StackTestData(".describeConstable().get()", "+", testString * MaxTtsNumber, "String") //,
      //StackTestData(".resolveConstantDesc(java.lang.invoke.MethodHandles.lookup()).toString", "+", testString * MaxTtsNumber, "String")
      //JDK16.0.1 java.lang.AssertionError: assertion failed: import failure: cannot determine unique overloaded method alternative from
      // def resolveConstantDesc(x$1: java.lang.invoke.MethodHandles.Lookup): String
      //def resolveConstantDesc(x$1: java.lang.invoke.MethodHandles.Lookup): Object
      // that matches method resolveConstantDesc:(x$1: java.lang.invoke.MethodHandles.Lookup): String
    )

    val StackTestsJDK15 = Seq(
      // StackTestData(accessor: String, aggregator: String, result: Any, resultType: String, implicitConversionShouldExist: Boolean = true)
      StackTestData(".stripIndent().length", "+", 3 * MaxTtsNumber, "Int"),
      StackTestData(".translateEscapes()", "+", testString * MaxTtsNumber, "String"),
      StackTestData(".formatted()", "+", testString * MaxTtsNumber, "String")
    )

    val javaSemanticVersion: SemanticVersion = SemanticVersion(JavaStringVersion)
    implicitExtensionOnTtsStacksTests(options, testValue = testValue, testValueType = "String", typecheckedImplicitMethod = "", implicitType = "",
      StackTestsJDK8 ++ // the following version comparing works since for JDK8 we get "1.8.0_291", for JDK9+ like "9.0.11"
       (if (javaSemanticVersion < "11.0.0") Seq() else StackTestsJDK11) ++
       (if (javaSemanticVersion < "12.0.0") Seq() else StackTestsJDK12) ++
       (if (javaSemanticVersion < "15.0.0") Seq() else StackTestsJDK15)
      : _*
    )

    behavior of "tms macros Scala Predef String methods implicits on Tagged String with TmsOptions" + options

    // --- Tagged String tests

    implicitExtensionOnTtsStacksTests(options, testValue = "\"123\".asInstanceOf[String @@ Int]", testValueType = "String", typecheckedImplicitMethod = "", implicitType = "",
      // StackTestData(accessor: String, aggregator: String, result: Any, resultType: String, implicitConversionShouldExist: Boolean = true)
      StackTestData(".concat(\"4\")", "+", "1234" * MaxTtsNumber, "String"),
      StackTestData(".indexOf(\"4\")", "+", -1 * MaxTtsNumber, "Int"),
      StackTestData(".indexOf('4')", "+", -1 * MaxTtsNumber, "Int"),
      StackTestData(".split('2').length", "+", 2 * MaxTtsNumber, "Int"),
      StackTestData(".lift(-1)", "orElse", None, "Option[Char]")
    )
  }
}
