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

import sands.sugar.tms.TmsImplicitsCommon.{@@, ctom}

import scala.annotation.compileTimeOnly
import scala.collection.{immutable, mutable}
import immutable.{StringOps, WrappedString}
import mutable.{ArrayOps, WrappedArray}
import scala.language.{higherKinds, implicitConversions}
import scala.runtime._

/*
 * Created by Serhiy Shamshetdinov
 * at 25.12.2020 18:23
 */

/**
 * This trait defines prioritized chain of implicits for transparent types of all supported depths to satisfy the type check.
 *
 * The magic is: after tmsFor macro replaces ttsNmmm call with the value of the desired type T (got in Fors Stack)
 * then the standard type check will apply corresponding natural Scala implicit (if required).
 *
 * `replaceByTtsImplicits` macro annotation replaces target abstract method definition
 * by transparent type implicits for max types stack depth of 5, like in the following example:
 *
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
private[tms] trait TmsImplicits extends TmsHigherPriorityImplicits {

  val fromScalaVersionIncluding: Option[String] = None
  val toScalaVersionExcluding: Option[String] = Some("2.12.13")

  // tms Highest priority implicits

  // This TmsImplicits trait *Identity implicits exist for some types that have Predef implicit extensions with overloads between them or
  // between extension and native class methods. Ones are the highest priority analogs of lowest priority general implicits `tts1(), ..., tts5()`
  // for the specific class and exist here to resolve native method overloads first.
  // These implicits are analogs of using class method without implicit (while general coding without tts), which are always applied first (only then - implicit extensions).
  // That's why *Identity implicits are only typechecker helpers and after removing by macros ones will not be replace by natural Scala implicits (native class method will be used).

  // following primitives ttsNxxx "*Identity" implicits are required to stop type checker misunderstanding of many possible variants of TmsPrimitivesWideningImplicits while applying, for example, + op to M1[Int]:
  //[error] src\main\scala\sands\monads\Main.scala:151:25: type mismatch;
  //[error]  found   : sands.monads.Main.optionInt.type (with underlying type Some[Int])
  //[error]  required: ?{def +(x$1: ? >: Int(1)): ?}
  //[error] Note that implicit conversions are not applicable because they are ambiguous:
  //[error]  both method tts1int2long in trait TmsPrimitivesConversionImplicits of type [T1[_], _](x: T1[Int])Long
  //[error]  and method tts1int2float in trait TmsPrimitivesConversionImplicits of type [T1[_], _](x: T1[Int])Float
  //[error]  are possible conversion functions from sands.monads.Main.optionInt.type to ?{def +(x$1: ? >: Int(1)): ?}
  //[error]   tmsFor[Option[Int]]()(optionInt + 1)
  // including other ttsN implicits above: tts1int2long, tts1int2float, tts1int2double - all are suitable for Some(Int).+ operation :)

  @replaceByTtsImplicits @compileTimeOnly(ctom) def byteIdentity[T <: Byte](x: T): Byte
  @replaceByTtsImplicits @compileTimeOnly(ctom) def shortIdentity[T <: Short](x: T): Short
  @replaceByTtsImplicits @compileTimeOnly(ctom) def intIdentity[T <: Int](x: T): Int
  @replaceByTtsImplicits @compileTimeOnly(ctom) def charIdentity[T <: Char](x: T): Char
  @replaceByTtsImplicits @compileTimeOnly(ctom) def longIdentity[T <: Long](x: T): Long
  @replaceByTtsImplicits @compileTimeOnly(ctom) def floatIdentity[T <: Float](x: T): Float
  @replaceByTtsImplicits @compileTimeOnly(ctom) def doubleIdentity[T <: Double](x: T): Double

  trait TmsStringImplicitExtensions { // valid for Java 16 inclusive
    // (1) Scala does not see the following overloads of String implicit extensions due to native String "overloads" exist with the same name but other signature.
    // It see only native String methods while tts overloads exist. That is why ones from implicit extensions are duplicated here.
    // (2) More over, one's calls with explicitly passed type do not compile at all even without tts, just try:
    // "123".lastIndexOf[AnyVal](50.0D) - does not compile, while "123".lastIndexOf(50.0D) - compiles & executes correctly. The error is:
    //overloaded method lastIndexOf with alternatives:
    //(x$1: String,x$2: Int)Int <and>
    //(x$1: String)Int <and>
    //(x$1: Int,x$2: Int)Int <and>
    //(x$1: Int)Int
    //does not take type parameters
    // Even when not specified in the input code these type parameters are added during typecheck and are seen after unlifting.
    // Tms macros drops unlifted type parameters of the listed String extensions while postprocessing.
    def split(separator: Char): Array[String]
    def split(separators: Array[Char]): Array[String]
    def startsWith[B](that: scala.collection.GenSeq[B], offset: Int): Boolean
    def startsWith[B](that: scala.collection.GenSeq[B]): Boolean
    def endsWith[B](that: scala.collection.GenSeq[B]): Boolean
    def indexOf[B >: Char](elem: B, from: Int): Int
    def indexOf[B >: Char](elem: B): Int;
    def lastIndexOf[B >: Char](elem: B, end: Int): Int
    def lastIndexOf[B >: Char](elem: B): Int
    def contains[A1 >: Char](elem: A1): Boolean
  }

  @replaceByTtsImplicits @compileTimeOnly(ctom) def stringIdentity[T <: String](x: T): String @@ TmsStringImplicitExtensions

  // Scala Predef implicit classes -----------------------------------------------------

  // it is here with priority upper then being in TmsHigherPriorityImplicits to resolve .length & .length() via arrayCharSequence, to be not ambiguous with genericArrayOps
  @replaceByTtsImplicits @compileTimeOnly(ctom) def arrayCharSequence[T <: Array[Char]](ac: T): Predef.ArrayCharSequence

  // refArrayOps implicit is the exact copy of scala.Predef.refArrayOps and is placed here to have higher priority than
  // ttsNgenericArrayOps has. This is required to resolve ambiguous implicits on Arrays with dimensions >= 2:
  // `def tts1genericArrayOps[T1[_] <: AnyRef, T](t: T1[Array[T]]): ArrayOps[T]`
  // is ambiguous with `Predef.refArrayOps[T <: AnyRef](xs: Array[T]): ArrayOps[T]`
  // for, for example, .map method on `Array[Array[Int]]`. Similarly for other ttsN depth
  implicit def refArrayOps[T <: AnyRef](xs: Array[T]): ArrayOps[T] = new ArrayOps.ofRef[T](xs)

}

private[tms] trait TmsHigherPriorityImplicits extends TmsMiddlePriorityImplicits {

  // wrapRefArray implicit is the exact copy of scala.LowPriorityImplicits.wrapRefArray and is placed here to have lower priority than
  // above TmsImplicits.refArrayOps has. This is also required to resolve ambiguous implicits on Arrays with dimensions >= 2.
  implicit def wrapRefArray[T <: AnyRef](xs: Array[T]): WrappedArray[T] = {
    if (xs eq null) null
    else if (xs.length == 0) WrappedArray.empty[T]
    else new WrappedArray.ofRef[T](xs)
  }

  // Scala Predef -----------------------------------------------------

  // Scala Predef implicit classes -----------------------------------------------------

  // implicit final class ArrowAssoc[A](private val self: A) extends AnyVal
  // ^ functionality stays as it is in scala.Predef since argument type is Any. To use transparent monad values
  // ^ as arguments one may manually cover the value with ttsN wrapper of desired depth to reach the inner value:
  // ^ `tts1(Some(1)) -> 2` will result `Some(1 -> 2)`

  // implicit final class Ensuring[A](private val self: A) extends AnyVal
  // ^ functionality stays as it is in scala.Predef since argument type is Any. To use transparent monad value
  // ^ as argument one may manually cover the value with ttsN wrapper of desired depth to reach the inner value:
  // ^ `tts1(Some(i)).ensuring(_ > 0)` will result `Some(i)` throwing Exception when i <= 0

  // implicit final class StringFormat[A](private val self: A) extends AnyVal
  // ^ functionality stays as it is in scala.Predef since argument type is Any. To use transparent monad value
  // ^ as argument one may manually cover the value with ttsN wrapper of desired depth to reach the inner value:
  // ^ `tts1(Some(1)).formatted("%3s")` will result `Some("  1")`

  // implicit final class any2stringadd[A](private val self: A) extends AnyVal
  // ^ functionality is controlled by the option and is realised by the tmsFor macro. Behaviour depends on "Predef Compliance" option flag

  // implicit final class RichException(private val self: Throwable) extends AnyVal
  // ^ functionality stays as it is in scala.Predef since extension method is deprecated. To use transparent monad value
  // ^ as argument one may manually cover the value with ttsN wrapper of desired depth to reach the inner value:
  // ^ `tts1(Some( new Exception() )).getStackTraceString.endsWith(scala.compat.Platform.EOL)` will result `Some(true)`

  // higher priority arrayCharSequence is defined in TmsImplicits
  @replaceByTtsImplicits @compileTimeOnly(ctom) def seqCharSequence[T <: IndexedSeq[Char]](isc: T): Predef.SeqCharSequence

  @replaceByTtsImplicits @compileTimeOnly(ctom) def augmentString[T <: String](t: T): StringOps
  @replaceByTtsImplicits @compileTimeOnly(ctom) def unaugmentString[T <: StringOps](t: T): String

  // Scala Predef views --------------------------------------------------------------

  //macros does not support tts implicits with 2 or more type variables
  //  implicit def tuple2ToZippedOps[T1, T2](x: (T1, T2))                           = new runtime.Tuple2Zipped.Ops(x)
  //  implicit def tuple3ToZippedOps[T1, T2, T3](x: (T1, T2, T3))                   = new runtime.Tuple3Zipped.Ops(x)

  // Scala Predef "Autoboxing" and "Autounboxing" ---------------------------------------------------

  @replaceByTtsImplicits @compileTimeOnly(ctom) def byte2Byte[T <: Byte](x: T): java.lang.Byte
  @replaceByTtsImplicits @compileTimeOnly(ctom) def short2Short[T <: Short](x: T): java.lang.Short
  @replaceByTtsImplicits @compileTimeOnly(ctom) def char2Character[T <: Char](x: T): java.lang.Character
  @replaceByTtsImplicits @compileTimeOnly(ctom) def int2Integer[T <: Int](x: T): java.lang.Integer
  @replaceByTtsImplicits @compileTimeOnly(ctom) def long2Long[T <: Long](x: T): java.lang.Long
  @replaceByTtsImplicits @compileTimeOnly(ctom) def float2Float[T <: Float](x: T): java.lang.Float
  @replaceByTtsImplicits @compileTimeOnly(ctom) def double2Double[T <: Double](x: T): java.lang.Double
  @replaceByTtsImplicits @compileTimeOnly(ctom) def boolean2Boolean[T <: Boolean](x: T): java.lang.Boolean

  @replaceByTtsImplicits @compileTimeOnly(ctom) def Byte2byte[T <: java.lang.Byte](x: T): Byte
  @replaceByTtsImplicits @compileTimeOnly(ctom) def Short2short[T <: java.lang.Short](x: T): Short
  @replaceByTtsImplicits @compileTimeOnly(ctom) def Character2char[T <: java.lang.Character](x: T): Char
  @replaceByTtsImplicits @compileTimeOnly(ctom) def Integer2int[T <: java.lang.Integer](x: T): Int
  @replaceByTtsImplicits @compileTimeOnly(ctom) def Long2long[T <: java.lang.Long](x: T): Long
  @replaceByTtsImplicits @compileTimeOnly(ctom) def Float2float[T <: java.lang.Float](x: T): Float
  @replaceByTtsImplicits @compileTimeOnly(ctom) def Double2double[T <: java.lang.Double](x: T): Double
  @replaceByTtsImplicits @compileTimeOnly(ctom) def Boolean2boolean[T <: java.lang.Boolean](x: T): Boolean
}

private[tms] trait TmsMiddlePriorityImplicits extends TmsLowerPriorityImplicits {

  // Scala Predef LowPriorityImplicits --------------------------------------------------------------

  // ones are here to:
  // - have higher priority than genericArrayOps & genericWrapArray for to, min, max methods in `Some(Array(1, 2)).min`: RichInt has more priority since it is inner
  // - have lower priority than Scala Predef "Autoboxing" and "Autounboxing"
  @replaceByTtsImplicits @compileTimeOnly(ctom) def byteWrapper[T <: Byte](x: T): RichByte
  @replaceByTtsImplicits @compileTimeOnly(ctom) def shortWrapper[T <: Short](x: T): RichShort
  @replaceByTtsImplicits @compileTimeOnly(ctom) def intWrapper[T <: Int](x: T): RichInt
  @replaceByTtsImplicits @compileTimeOnly(ctom) def charWrapper[T <: Char](c: T): RichChar
  @replaceByTtsImplicits @compileTimeOnly(ctom) def longWrapper[T <: Long](x: T): RichLong
  @replaceByTtsImplicits @compileTimeOnly(ctom) def floatWrapper[T <: Float](x: T): RichFloat
  @replaceByTtsImplicits @compileTimeOnly(ctom) def doubleWrapper[T <: Double](x: T): RichDouble
  @replaceByTtsImplicits @compileTimeOnly(ctom) def booleanWrapper[T <: Boolean](x: T): RichBoolean
}

private[tms] trait TmsLowerPriorityImplicits extends TmsLowestPriorityImplicits {

  // Scala Predef views --------------------------------------------------------------

  // it is here to:
  // - have higher priority than genericWrapArray like Scala Predef has
  // - have lower priority than Scala Predef primitives Wrappers to Rich*
  @replaceByTtsImplicits @compileTimeOnly(ctom) def genericArrayOps[T](t: Array[T]): ArrayOps[T]
}

private[tms] trait TmsLowestPriorityImplicits {

  // Scala Predef LowPriorityImplicits --------------------------------------------------------------

  @replaceByTtsImplicits @compileTimeOnly(ctom) def genericWrapArray[T](t: Array[T]): WrappedArray[T]

  @replaceByTtsImplicits @compileTimeOnly(ctom) def wrapString[T <: String](s: T): WrappedString
  @replaceByTtsImplicits @compileTimeOnly(ctom) def unwrapString[T <: WrappedString](ws: T): String

  // tms general implicits ---------------------------------------------------

  // Stack set of the most general implicits ttsN "N Transparent TypeS": to satisfy compiler typecheck. Ones have the lowest priority of all implicits defined in this file.
  // Calls to ttsN (like all others ttsNmmm calls) are replaced by the inner values of `monad` "opened" to Nth level in Fors Stack built by macros.
  // Among tts1-tts5: in general case tts5 has highest priority while tts1 - lowest (Scala implementation). That's why when several _tts_ implicit overloads
  // exists on tms stack the most inner type overload is selected like in the following example: `Some(List("1", "22", "333")).length` will have type `Some[List[Int]]`
  // resulting `Some(List(1, 2, 3))` with tts2 implicit applied: `tts2( Some(List("1", "22", "333")) ).length` - .length is tiered to most inner type which has .length accessor
  // contrarily to also possible tts1 implicit overload to evaluate Some[Int] with .length applied to List.
  // I see: parameter type of tts2 `T1[ T2[T] ]` is "as specific as" parameter type of tts1 `T1[ T ]` but not vice versa (tts2 has +1, tts1 - 0).
  // So since both implicits reside in one class (+0:+0) then tts2 is "more specific than" tts1 (1:0) and is selected.
  // Other pairs priority relations - similarly. See https://www.scala-lang.org/files/archive/spec/2.12/06-expressions.html#overloading-resolution
  // Other tts stacks defined have the same property: tts5mmm() has highest priority in the stack while tt1mmm - lowest (for example, genericWrapArray for nested Arrays)
  // Priorities among tts groups are always pulled up to the most inner type of the stack (tts5 of one tts implicits group has higher priority than tts4 of other tts implicits group)
  // with the only 1 exception: Array collection implicits all has higher priority than any other collection has via these tts1-tts5 general implicits.
  // This is the Scala implementation and it happens because Arrays has implicits in Predef.
  // This means that in `Some(Array(Seq(1,2))).length` the length of the Array will be calculated, not the array of the more inner Seq.
  // To access .length of the Seq explicitly wrap to ttsN of required depth. Here: `tts2(Some(Array(Seq(1,2)))).length`

  @compileTimeOnly(ctom) implicit def tts1[T1[_] <: AnyRef, T](t: T1[T]): T = ???
  @compileTimeOnly(ctom) implicit def tts2[T1[_] <: AnyRef, T2[_] <: AnyRef, T](t: T1[T2[T]]): T = ???
  @compileTimeOnly(ctom) implicit def tts3[T1[_] <: AnyRef, T2[_] <: AnyRef, T3[_] <: AnyRef, T](t: T1[T2[T3[T]]]): T = ???
  @compileTimeOnly(ctom) implicit def tts4[T1[_] <: AnyRef, T2[_] <: AnyRef, T3[_] <: AnyRef, T4[_] <: AnyRef, T](t: T1[T2[T3[T4[T]]]]): T = ???
  @compileTimeOnly(ctom) implicit def tts5[T1[_] <: AnyRef, T2[_] <: AnyRef, T3[_] <: AnyRef, T4[_] <: AnyRef, T5[_] <: AnyRef, T](t: T1[T2[T3[T4[T5[T]]]]]): T = ???

  // scala.Byte implicits ---------------------------------------------------

  @replaceByTtsImplicits @compileTimeOnly(ctom) def byte2short[T <: Byte](x: T): Short
  @replaceByTtsImplicits @compileTimeOnly(ctom) def byte2int[T <: Byte](x: T): Int
  @replaceByTtsImplicits @compileTimeOnly(ctom) def byte2long[T <: Byte](x: T): Long
  @replaceByTtsImplicits @compileTimeOnly(ctom) def byte2float[T <: Byte](x: T): Float
  @replaceByTtsImplicits @compileTimeOnly(ctom) def byte2double[T <: Byte](x: T): Double

  // scala.Short implicits ---------------------------------------------------

  @replaceByTtsImplicits @compileTimeOnly(ctom) def short2int[T <: Short](x: T): Int
  @replaceByTtsImplicits @compileTimeOnly(ctom) def short2long[T <: Short](x: T): Long
  @replaceByTtsImplicits @compileTimeOnly(ctom) def short2float[T <: Short](x: T): Float
  @replaceByTtsImplicits @compileTimeOnly(ctom) def short2double[T <: Short](x: T): Double

  // scala.Int implicits ---------------------------------------------------

  @replaceByTtsImplicits @compileTimeOnly(ctom) def int2long[T <: Int](x: T): Long
  @replaceByTtsImplicits @compileTimeOnly(ctom) def int2float[T <: Int](x: T): Float
  @replaceByTtsImplicits @compileTimeOnly(ctom) def int2double[T <: Int](x: T): Double

  // scala.Char implicits ---------------------------------------------------

  @replaceByTtsImplicits @compileTimeOnly(ctom) def char2int[T <: Char](x: T): Int
  @replaceByTtsImplicits @compileTimeOnly(ctom) def char2long[T <: Char](x: T): Long
  @replaceByTtsImplicits @compileTimeOnly(ctom) def char2float[T <: Char](x: T): Float
  @replaceByTtsImplicits @compileTimeOnly(ctom) def char2double[T <: Char](x: T): Double

  // scala.Long implicits ---------------------------------------------------

  @replaceByTtsImplicits @compileTimeOnly(ctom) def long2float[T <: Long](x: T): Float
  @replaceByTtsImplicits @compileTimeOnly(ctom) def long2double[T <: Long](x: T): Double

  // scala.Float implicits ---------------------------------------------------

  @replaceByTtsImplicits @compileTimeOnly(ctom) def float2double[T <: Float](x: T): Double
}
