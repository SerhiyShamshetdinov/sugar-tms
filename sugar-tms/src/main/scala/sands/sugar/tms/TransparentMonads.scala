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
 * at 27.11.2020 15:17
 */

import scala.annotation.StaticAnnotation
import scala.language.experimental.macros
import scala.language.{higherKinds, implicitConversions}

object TransparentMonads extends TmsImplicits {

  /** Specifies tms options for all macro calls enclosed by an annotated symbol.
   *
   * Available options are:
   *  - "Debug" / "No Debug"                                                             ( "D" / "ND"            ) enables / disables macros debug output
   *  - "Trace" / "No Trace"                                                             ( "T" / "NT"            ) enables / disables macros trace output
   *  - "Predef Compliance" / "No Predef Compliance"                                     ( "PC" / "NPC"          ) enables / disables any2stringadd predef compliance. When enabled `Some(1) + "2"` is `"Some(1)2"`, when disabled - `Some("12")`
   *  - "Fors Stack For Apply Source" / "No Fors Stack For Apply Source"                 ( "FSFAS" / "NFSFAS"    ) enables / disables building apply source containing ttsN in a separate inner Fors Stack
   *  - "Fors Stack For Apply Parameter" / "No Fors Stack For Apply Parameter"           ( "FSFAP" / "NFSFAP"    ) enables / disables building apply parameter containing ttsN in a separate inner Fors Stack
   *  - "Embedded Fors Code View" / "No Embedded Fors Code View"                         ( "EFCV" / "NEFCV"      ) enables / disables embedding of the string containing fors-view of the macro output to macro result as first local val in the output block
   *  - "Single Fors Stack"                                                              ( "SFS"                 ) equivalent to applying "No Fors Stack For Apply Source" and "No Fors Stack For Apply Parameter" options at the same time
   *  - "All Fors Stacks"                                                                ( "AFS"                 ) equivalent to applying "Fors Stack For Apply Source" and "Fors Stack For Apply Parameter" options at the same time
   *  - "Pre Evaluate No Types"                                                          ( "PENT"                ) disables pre evaluation for all Fors Stack types
   *  - "Pre Evaluate All Types"                                                         ( "PEAT"                ) enables pre evaluation for all Fors Stack types
   *  - "Pre Evaluate Monadic Flow Type"                                                 ( "PEMFT"               ) for *mfc()/mfcFor()()* macro calls it enables pre evaluation of the detected Monadic Flow type
   *  - "Pre Evaluate ''class_name/FQN'', ..."                                           ( "PE ''cn/FQN'', ..."  ) adds the list of class names or fully qualified names to the set of pre evaluated types. For example, `"PE Future, scala.util.Try"`
   *
   * @param tmsLiteralOptions list of literal strings with full or abbreviated tms options above ignoring case and trimming spaces. All options may be passed in one string separated by ';' like "D; T"
   */
  class tmsOptions(tmsLiteralOptions: String*) extends StaticAnnotation

  /** Version of [[tmsFor]] macro without tms options parameter */
  def tms[O](tmsCode: Any): O = macro TransparentMonadsImpl.transparentMonadsImpl[O]

  /** Full name version of [[tms]] macro */
  def transparentMonads[O](tmsCode: Any): O = macro TransparentMonadsImpl.transparentMonadsImpl[O]

  /** Transform input code replacing ttsNxxx implicits by values extracted in the Fors Stack of resulting type O
   *
   * @param tmsLiteralOptions list of literal string tms options applied for this macro call. @see [[tmsOptions list of available options]]
   * @param tmsCode input code with ttsNxxx implicits
   * @tparam O type of the output code and built Fors Stack
   * @return tms transformed code with built Fors Stack
   */
  def tmsFor[O](tmsLiteralOptions: String*)(tmsCode: Any): O = macro TransparentMonadsImpl.transparentMonadsForImpl[O]

  /** Full name version of [[tmsFor]] macro */
  def transparentMonadsFor[O](tmsLiteralOptions: String*)(tmsCode: Any): O = macro TransparentMonadsImpl.transparentMonadsForImpl[O]

  /** Version of [[mfcFor]] macro without tms options parameter */
  def mfc[O](tmsCode: Any): O = macro MonadicFlowControlImpl.transparentMonadsImpl[O]

  /** Full name version of [[mfc]] macro */
  def monadicFlowControl[O](tmsCode: Any): O = macro MonadicFlowControlImpl.transparentMonadsImpl[O]

  /** Along with [[tmsFor]] transformations it treats input code as Monadic Flow of the type detected by the first expression or val definition.
   *
   * @param tmsLiteralOptions list of literal string tms options applied for this macro call. @see [[tmsOptions list of available options]]
   * @param tmsCode input Monadic Flow code possibly containing ttsNxxx implicits
   * @tparam O type of the output code and built Fors Stack
   * @return mfc transformed code with built Fors Stack
   */
  def mfcFor[O](tmsLiteralOptions: String*)(tmsCode: Any): O = macro MonadicFlowControlImpl.transparentMonadsForImpl[O]

  /** Full name version of [[mfcFor]] macro */
  def monadicFlowControlFor[O](tmsLiteralOptions: String*)(tmsCode: Any): O = macro MonadicFlowControlImpl.transparentMonadsForImpl[O]
}
