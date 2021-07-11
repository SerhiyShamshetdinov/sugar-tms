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

import scala.reflect.macros.blackbox

/*
 * Created by Serhiy Shamshetdinov
 * at 16.12.2020 13:14
 */

/** Replaces TmsTree nodes/subtrees by simplified one or by functions for "Predef Compliance" option.
 * This is "preprocessing" for further extraction
 */
private[tms] trait TmsTreePreprocessing extends TmsTools {
  trait TmsTreePreprocessor[T] {
    def preprocess(source: T): T
  }

  implicit class TmsTreePreprocessorSyntax[T](source: T)(implicit preprocessor: TmsTreePreprocessor[T]) {
    def preprocess: T = preprocessor.preprocess(source)
  }

  object TmsTreePreprocessor {
    implicit def seqTreePreprocessor[T](implicit lemPreprocessor: TmsTreePreprocessor[T]): TmsTreePreprocessor[Seq[T]] =
      new TmsTreePreprocessor[Seq[T]] {
        def preprocess(source: Seq[T]): Seq[T] = source.map(lemPreprocessor.preprocess)
      }
  }
}

