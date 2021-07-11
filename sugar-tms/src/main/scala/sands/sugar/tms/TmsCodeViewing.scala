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

/** Builds source code of the passed source
 */
private[tms] trait TmsCodeViewing {
  val c: blackbox.Context
  import c.universe._

  private val SpacesInIdent = 2
  private val SpaceIdent: String = " " * SpacesInIdent

  def withIdent(linesString: String): String = linesString.linesIterator.map(SpaceIdent + _).mkString("\n") // lines

  def block(body: String): String = s"{\n${withIdent(body)}\n}"

  trait TmsCodeViewer[S] {
    def codeView(source: S): String
  }

  implicit class TmsCodeViewerSyntax[T](source: T)(implicit tmsCodeViewer: TmsCodeViewer[T]) {
    def codeView: String = tmsCodeViewer.codeView(source)
  }

  object TmsCodeViewer {
    implicit val treeCodeViewer: TmsCodeViewer[Tree] = new TmsCodeViewer[Tree] {
      def codeView(source: Tree): String = source.toString // showCode(source) FAILS !!! to show full code (at least in 2.11)
    }
  }
}

