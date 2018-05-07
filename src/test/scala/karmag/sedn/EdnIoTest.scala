package karmag.sedn

import java.io.StringWriter

import karmag.sedn.Edn._
import org.scalatest.FunSuite

import scala.util.{Failure, Success}

class EdnIoTest extends FunSuite {

  private def read(string: String): Edn =
    EdnIo.read(string)() match {
      case Some(maybe) =>
        maybe match {
          case Success(edn) => edn
          case Failure(error) => throw error
        }
      case None => throw new Exception("Should contain one value")
    }

  test("atoms") {
    assert(read("nil") === ENil)

    assert(read("true") === EBool(true))
    assert(read("false") === EBool(false))

    assert(read("\"text\"") === EString("text"))

    assert(read("\\c") === EChar('c'))
    assert(read("\\newline") === EChar('\n'))
    assert(read("\\tab") === EChar('\t'))

    assert(read("1") === EInt(1))
    assert(read("1N") === EInt(1))
    assert(read("123123123123123123N") === EInt(123123123123123123l))

    assert(read("1.0") === EDecimal(BigDecimal.apply("1.0")))
    assert(read("1.123123123123123123M") === EDecimal(BigDecimal.apply("1.123123123123123123")))

    assert(read("1/2") === ERatio(1, 2))
    assert(read("-5/11") === ERatio(-5, 11))

    assert(read("symbol") === ESymbol("symbol"))
    assert(read("namespace/symbol") === ESymbol("namespace/symbol"))

    assert(read(":kw") === EKeyword("kw"))
    assert(read(":namespace/kw") === EKeyword("namespace/kw"))
  }

  test("collections") {
    assert(read("(1 2 3)") === EList(List(EInt(1), EInt(2), EInt(3))))
    assert(read("[1 2 3]") === EVector(Vector(EInt(1), EInt(2), EInt(3))))
    assert(read("{:a 1 :b 2}") === EMap(Map(EKeyword("a") -> EInt(1), EKeyword("b") -> EInt(2))))
    assert(read("#{:a 1}") === ESet(Set(EKeyword("a"), EInt(1))))
  }

  test("other") {
    assert(read("#tag value") === ETag(ESymbol("tag"), ESymbol("value")))
  }

  test("errors") {
    val resultOpt = EdnIo.read("}")()
    assert(resultOpt.isDefined, "There should be a value")

    val result = resultOpt.get
    assert(result.isFailure, "Reading should have failed")

    val error = result match { case Failure(t) => t; case _ => null }
    assert(error.getMessage === "Unmatched delimiter: }")
  }

  test("write") {
    val text = """[1 "two" (:three four) {:a 1} #{true} #tag value]"""
    val edn = read(text)
    val sw = new StringWriter()
    EdnIo.write(sw, edn)

    assert(text === sw.toString)
    assert(text === EdnIo.compactString(edn))
  }

  test("pprint - list/vector") {
    val delims = List("(" -> ")", "[" -> "]")

    // short data
    delims.foreach { case (start, end) =>
      val input = s"$start${1.to(5).mkString(" ")}$end"
      val edn = read(input)
      val output = EdnIo.prettyString(edn)
      assert(input === output)
    }

    // long data
    delims.foreach { case (start, end) =>
      val input = s"$start${1.to(1000).mkString(" ")}$end"
      val edn = read(input)
      val output = EdnIo.prettyString(edn)
      assert(output === s"$start\n  ${1.to(1000).mkString("\n  ")}\n$end")
    }
  }

  test("pprint - tag") {
    val input = "#a #b hello"
    val edn = read(input)
    val output = EdnIo.prettyString(edn)
    assert(input === output)
  }

  test("pprint - map") {
    val items = List(
      """{:a 1}""",
     """{
        |  some-long-long-long-long-long-long-long-long-long-long-long-long-long-long-long-key
        |  some-long-long-long-long-long-long-long-long-long-long-long-long-long-long-long-value
        |}""".stripMargin,
     """{
        |  key
        |    [
        |      abcdefghijklmnopqrstuvxyz
        |      abcdefghijklmnopqrstuvxyz
        |      abcdefghijklmnopqrstuvxyz
        |      abcdefghijklmnopqrstuvxyz
        |      abcdefghijklmnopqrstuvxyz
        |      abcdefghijklmnopqrstuvxyz
        |      abcdefghijklmnopqrstuvxyz
        |      abcdefghijklmnopqrstuvxyz
        |    ]
        |}""".stripMargin,
     """{
        |  [
        |    abcdefghijklmnopqrstuvxyz
        |    abcdefghijklmnopqrstuvxyz
        |    abcdefghijklmnopqrstuvxyz
        |    abcdefghijklmnopqrstuvxyz
        |    abcdefghijklmnopqrstuvxyz
        |    abcdefghijklmnopqrstuvxyz
        |    abcdefghijklmnopqrstuvxyz
        |  ]
        |    value
        |}""".stripMargin
    )

    items.foreach { input =>
      val edn = read(input)
      val output = EdnIo.prettyString(edn)
      assert(input === output)
    }
  }
}
