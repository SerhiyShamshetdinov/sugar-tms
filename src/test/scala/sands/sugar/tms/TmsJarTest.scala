package sands.sugar.tms

import sands.sugar.tms.TransparentMonads._

/*
 * Created by Serhiy Shamshetdinov
 * at 24.06.2021 13:46
 */

@tmsOptions("No Debug", "NT")
class TmsJarTest extends TmsTestBase {

  behavior of "tms jar"

  it should "export transparentMonads macro variants: NON-TOOLBOX" in {
    transparentMonads[Option[Int]](Some(1) + 2) shouldBe Some(3)
    tms[Option[Int]](if (Some(true)) 1 else 0) shouldBe Some(1)

    transparentMonadsFor[Option[Int]]("EFCV")(identity[Int](Some(1))) shouldBe Some(1)
    tmsFor[Option[Int]]("Embedded Fors Code View")(Some(1) + Some(2)) shouldBe Some(3)
  }

  it should "export transparentMonads macro variants" in new ToolboxTest {
    val options: String = Seq[String]("Embedded Fors Code View").mkString("\"", "\", \"", "\"")

    val testCode: String =
      s"""$ToolboxTestImports
         |
         |Seq[Option[Int]](
         |
         |transparentMonads[Option[Int]](Some(1) + 2),
         |tms[Option[Int]](if (Some(true)) 1 else 0),
         |
         |transparentMonadsFor[Option[Int]]($options)(identity[Int](Some(1))),
         |tmsFor[Option[Int]]($options)(Some(1) + Some(2))
         |
         |).reduce[Option[Int]]((oi1, oi2) => tms[Option[Int]](oi1 + oi2))
         |
         |""".stripMargin

    evaluateCode(testCode) shouldBe Some(8)
  }

  it should "export monadicFlowControl macro variants: NON-TOOLBOX" in {
    monadicFlowControl[Option[Int]] {
      val si = Some(1)
      si + 2
    } shouldBe Some(3)
    mfc[Option[Int]] {
      val sb = Some(false)
      if (sb) 1 else 0
    } shouldBe Some(0)

    monadicFlowControlFor[Option[Int]]("EFCV") {
      val sb = Some(false)
      if (sb || true) 1 else 0
    } shouldBe Some(1)
    mfcFor[Option[Int]]("Embedded Fors Code View") {
      val si1 = Some(1)
      val si2 = Some(2)
      si1 + si2
    } shouldBe Some(3)
  }

  it should "export monadicFlowControl macro variants" in new ToolboxTest {
    val options: String = Seq[String]("Embedded Fors Code View").mkString("\"", "\", \"", "\"")

    val testCode: String =
      s"""$ToolboxTestImports
         |
         |Seq[Option[Int]](
         |
         |monadicFlowControl[Option[Int]] {
         |  val si = Some(1)
         |  si + 2
         |},
         |mfc[Option[Int]] {
         |  val sb = Some(false)
         |  if (sb) 1 else 0
         |},
         |
         |monadicFlowControlFor[Option[Int]]($options) {
         |  val sb = Some(false)
         |  if (sb || true) 1 else 0
         |},
         |mfcFor[Option[Int]]($options) {
         |  val si1 = Some(1)
         |  val si2 = Some(2)
         |  si1 + si2
         |}
         |
         |).reduce[Option[Int]]((oi1, oi2) => tms[Option[Int]](oi1 + oi2))
         |
         |""".stripMargin

    evaluateCode(testCode) shouldBe Some(7)
  }
}
