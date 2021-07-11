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
 * at 15.05.2021 17:30
 */

object TmsImplicitsCommon { // it is public for toolbox tests
  type @@[A, T] = A with T

  val ctom: String = "Transparent Types implicits may only be used in Transparent Monads or Monadic Flow Control macros input code " +
    "as helpers to satisfy compiler type check and designate expressions that should be extracted by macro in the built for-s stack. " +
    "If you see this error then either you should wrap the containing part of the code by a macro, " +
    "reduce implicits import scope or there is your error in this code. If you are adding (+ or $plus) String and Any value v " +
    "outside the macro then you should wrap this Any value v by String.valueOf(v) method to skip ttsNxxx implicit applying. " +
    "Reducing the scope of TransparentMonads import closer to the macro call also helps in above or similar cases"
}

