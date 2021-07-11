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

import scala.language.higherKinds
import scala.util.Try

/*
 * Created by Serhiy Shamshetdinov
 * at 18.02.2021 18:38
 */

trait TmsForTypeClassesTests extends TmsTestBase {

  trait MonadLike[M[_]] {
    def pure[T](v: T): M[T]
    def map[A, B](v: M[A])(f: A => B): M[B]
    def flatMap[A, B](v: M[A])(f: A => M[B]): M[B]
  }

  object MonadLike {
    def apply[M[_]](implicit ml: MonadLike[M]): MonadLike[M] = ml

    implicit val optionMonadLike: MonadLike[Option] = new MonadLike[Option] {
      def pure[T](v: T): Option[T] = Option(v)
      def map[A, B](v: Option[A])(f: A => B): Option[B] = v.map(f)
      def flatMap[A, B](v: Option[A])(f: A => Option[B]): Option[B] = v.flatMap(f)
    }

    implicit val tryMonadLike: MonadLike[Try] = new MonadLike[Try] {
      def pure[T](v: T): Try[T] = Try(v)
      def map[A, B](v: Try[A])(f: A => B): Try[B] = v.map(f)
      def flatMap[A, B](v: Try[A])(f: A => Try[B]): Try[B] = v.flatMap(f)
    }
  }

  implicit class ForOps[M[_]: MonadLike, A](v: M[A]) {
    def map[B](f: A => B): M[B] = MonadLike[M].map(v)(f)
    def flatMap[B](f: A => M[B]): M[B] = MonadLike[M].flatMap(v)(f)
  }

  def typeClassesTests(tmsOptions: Seq[String]): Unit = {
    val options = literalOptions(tmsOptions)

    behavior of "tms macro with Type Classes with TmsOptions" + options

    //TOOLBOX tests errors: reflective compilation has failed:
    //ambiguous reference to overloaded definition,
    //both method ForOps of type [M[_], A](v: M[A])(implicit evidence$1: MonadLike[M]): ForOps[M,A]
    //and  method ForOps of type [M[_], A](v: M[A])(implicit evidence$1: MonadLike[M]): ForOps[M,A]
    //match argument types (M1[M2[Int]])
    //method ForOps is defined twice;
    //  the conflicting method ForOps was defined at line 30:16
    //scala.tools.reflect.ToolBoxError: reflective compilation has failed:

    it should "process single tts1 with type classes: NON-TOOLBOX" in {
      import sands.sugar.tms.TransparentMonads._

      def plusOne[M1[_] <: AnyRef: MonadLike](num: Int): M1[Int] =
        tmsFor[M1[Int]]()(
          MonadLike[M1].pure(1) + num
        )

      plusOne[Try](2)     shouldBe Try(3)
      plusOne[Option](2)  shouldBe Some(3)
    }

    it should "process operation on 2 tts1 of the same type class: NON-TOOLBOX" in {
      import sands.sugar.tms.TransparentMonads._

      def add[M1[_] <: AnyRef: MonadLike](num1: Int, num2: Int): M1[Int] =
        tmsFor[M1[Int]]()(
          MonadLike[M1].pure(num1) + MonadLike[M1].pure(num2)
        )

      add[Try](1, 2)    shouldBe Try(3)
      add[Option](1, 2) shouldBe Some(3)
    }

    it should "process operation on 2 tts1 of different type classes: NON-TOOLBOX" in {
      import sands.sugar.tms.TransparentMonads._

      def add[M1[_] <: AnyRef: MonadLike, M2[_] <: AnyRef: MonadLike](num1: Int, num2: Int): M1[M2[Int]] =
        tmsFor[M1[M2[Int]]]()(
          MonadLike[M1].pure(num1) + MonadLike[M2].pure(num2)
        )

      add[Try, Option](1, 2)    shouldBe Try(Some(3))
      add[Option, Try](1, 2)    shouldBe Some(Try(3))
      add[Option, Option](1, 2) shouldBe Some(Some(3))
      add[Try, Try](1, 2)       shouldBe Try(Try(3))
    }

    it should "process operation on 2 tts1 with type & concrete classes: NON-TOOLBOX" in {
      import sands.sugar.tms.TransparentMonads._

      def add[M1[_] <: AnyRef: MonadLike](maybeNum: Option[Int], num: Int): M1[Option[Int]] =
        tmsFor[M1[Option[Int]]]()(
          MonadLike[M1].pure(num) + maybeNum
        )

      add[Option](Some(1), 2) shouldBe Some(Some(3))
      add[Try](Some(1), 2)    shouldBe Try(Some(3))
      add[Try](None, 2)     shouldBe Try(None)
    }

    //2.11 compilation error:
    // can't existentially abstract over parameterized type M2
    //[error]     it should "process tts2 with 2 type classes: NON-TOOLBOX" in {
//    it should "process tts2 with 2 type classes: NON-TOOLBOX" in {
//      import sands.sugar.tms.TransparentMonads._
//
//      class Test1[M1[_] <: AnyRef: MonadLike] {
//        def test2[M2[_] <: AnyRef: MonadLike]: M1[M2[Int]] =
//          tmsFor[M1[M2[Int]]]()(MonadLike[M1].pure(MonadLike[M2].pure(3)) + 1)
//
//        def test: M1[Option[Int]] = test2[Option]
//      }
//
//      new Test1[Try].test shouldBe Try(Some(4))
//      new Test1[Option].test shouldBe Some(Some(4))
//    }

    it should "process tts2 with 2 type classes: NON-TOOLBOX" in {
      import sands.sugar.tms.TransparentMonads._

      def test[M1[_] <: AnyRef: MonadLike, M2[_] <: AnyRef: MonadLike]: M1[M2[Int]] =
        tmsFor[M1[M2[Int]]]()(MonadLike[M1].pure(MonadLike[M2].pure(3)) + 1)


      test[Try, Option]     shouldBe Try(Some(4))
      test[Option, Option]  shouldBe Some(Some(4))
      test[Option, Try]     shouldBe Some(Try(4))
      test[Try, Try]        shouldBe Try(Try(4))
    }

    it should "process tts with alias with type classes: NON-TOOLBOX" in {
      import sands.sugar.tms.TransparentMonads._

      type MonadicInt = Option[Int]

      def plusOne[M1[_] <: AnyRef: MonadLike](num: MonadicInt): M1[MonadicInt] =
        tmsFor[M1[MonadicInt]]()(
          MonadLike[M1].pure(1) + num
        )

      plusOne[Try](Some(1)) shouldBe Try(Some(2))
      plusOne[Try](None) shouldBe Try(None)
      plusOne[Option](Some(1)) shouldBe Some(Some(2))
    }

  }
}
