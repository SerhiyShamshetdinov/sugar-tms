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
 * at 18.07.2021 19:45
 */

private[tms] case class SemanticVersion(major: Int, minor: Int, patch: Int) {
  private def cmpOther(other: String, cmp: (Int, Int) => Boolean): Boolean = {
    val otherSV = SemanticVersion(other)
    cmp(major, otherSV.major) || major == otherSV.major && (
      cmp(minor, otherSV.minor) || minor == otherSV.minor &&
        cmp(patch, otherSV.patch)
      )
  }
  private val lessThan: String => Boolean = cmpOther(_, _ < _)
  private val greaterThan: String => Boolean = cmpOther(_, _ > _)

  def <(other: String): Boolean = lessThan(other)
  def >=(other: String): Boolean = !lessThan(other)
  def >(other: String): Boolean = greaterThan(other)
  def <=(other: String): Boolean = !greaterThan(other)
}

private[tms] object SemanticVersion {
  private val Pattern = """^(\d+)\.(\d+)\.(\d+).*""".r

  def apply(version: String): SemanticVersion = version match {
    case Pattern(major, minor, patch) => new SemanticVersion(major.toInt, minor.toInt, patch.toInt)
    case _ => throw new IllegalArgumentException(s"Input version string '$version' does not match version pattern '$Pattern'")
  }
}
