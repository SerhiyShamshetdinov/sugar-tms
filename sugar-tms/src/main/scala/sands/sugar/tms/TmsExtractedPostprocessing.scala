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
 * at 13.01.2021 16:00
 */

/** Defines the postprocessing actions on the extracted TmsExtracted tree nodes to optimize the output
 */
private[tms] trait TmsExtractedPostprocessing { _: TmsForExtraction =>

  trait TmsExtractedPostprocessor[T] {
    def postprocess(source: T)(implicit context: TmsExtractionContext): T
  }

  implicit class TmsExtractedPostprocessorSyntax[T](source: T)(implicit tmsExtractedPostprocessor: TmsExtractedPostprocessor[T]) {
    def postprocess(implicit context: TmsExtractionContext): T = tmsExtractedPostprocessor.postprocess(source)
  }

}

