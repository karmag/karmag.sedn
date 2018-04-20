package karmag.sedn

import karmag.sedn.Edn.{EVector, Edn}
import karmag.sedn.EdnPath.PathError
import karmag.sedn.TestUtil.read
import org.scalatest.FunSpec

class EdnPathTest extends FunSpec {

  private def get(data: String, key: String): Either[PathError, Edn] =
    EdnPath.getEither(
      read(data),
      EdnPath.EdnPath(read(key) match { case EVector(vec) => vec.toList; case _ => null })
    )

  private def set(data: String, key: String, value: String): Either[PathError, Edn] =
    EdnPath.setEither(
      read(data),
      EdnPath.EdnPath(read(key) match { case EVector(vec) => vec.toList; case _ => null }),
      read(value)
    )

  private def errorContains(error: Either[PathError, Edn], msg: String): Boolean =
    error match {
      case Left(pe) =>
        if (pe.message.contains(msg))
          true
        else
          throw new Exception(s"Expected [$msg] got [${pe.message}]")
      case _ =>
        throw new Exception("Not an error")
    }

  describe("nil") {
    it("get") {
      assert(get("nil", "[a]") === Right(read("nil")))
      assert(get("nil", "[a b c]") === Right(read("nil")))
    }

    it("update/set") {
      assert(set("nil", "[a b]", "2") === Right(read("{a {b 2}}")))
    }
  }

  describe("list") {
    it("get") {
      assert(get("(a b)", "[0]") === Right(read("a")))
      assert(get("(a b)", "[1]") === Right(read("b")))
      assert(get("(a b)", "[2]") === Right(read("nil")))

      assert(errorContains(get("(a b)", "[a]"), "List key must be integer"))
    }

    it("update/set") {
      assert(set("(a b)", "[0]", "101") === Right(read("(101 b)")))
      assert(set("(a b)", "[1]", "101") === Right(read("(a 101)")))
      assert(set("(a b)", "[2]", "101") === Right(read("(a b 101)")))
      assert(set("(a b)", "[3]", "101") === Right(read("(a b nil 101)")))

      assert(errorContains(set("(a b)", "[a]", "1"), "List key must be integer"))
    }
  }

  describe("vector") {
    it("get") {
      assert(get("[a b]", "[0]") === Right(read("a")))
      assert(get("[a b]", "[1]") === Right(read("b")))
      assert(get("[a b]", "[2]") === Right(read("nil")))

      assert(errorContains(get("[a b]", "[a]"), "Vector key must be integer"))
    }

    it("update/set") {
      assert(set("[a b]", "[0]", "101") === Right(read("[101 b]")))
      assert(set("[a b]", "[1]", "101") === Right(read("[a 101]")))
      assert(set("[a b]", "[2]", "101") === Right(read("[a b 101]")))
      assert(set("[a b]", "[3]", "101") === Right(read("[a b nil 101]")))

      assert(errorContains(set("[a b]", "[a]", "1"), "Vector key must be integer"))
    }
  }

  describe("map") {
    it("get") {
      assert(get("{a 1}", "[a]") === Right(read("1")))
      assert(get("{a 1}", "[x]") === Right(read("nil")))
      assert(get("{a {b 1}}", "[a b]") === Right(read("1")))
    }

    it("update/set") {
      assert(set("{a 1}", "[a]", "2") === Right(read("{a 2}")))
      assert(set("{a {b 1}}", "[a b]", "2") === Right(read("{a {b 2}}")))
    }
  }

  describe("set") {
    it("get") {
      assert(get("#{1 2}", "[1]") === Right(read("1")))
      assert(get("#{1 2}", "[3]") === Right(read("nil")))
      assert(get("#{1 #{2}}", "[#{2} 2]") === Right(read("2")))
    }

    it("update/set") {
      assert(set("#{a}", "[a]", "b") === Right(read("#{b}")))
      assert(set("#{a}", "[b]", "c") === Right(read("#{a c}")))
      assert(set("#{a #{b}}", "[#{b} b]", "c") === Right(read("#{a #{c}}")))
    }
  }

  describe("tag") {
    it("get") {
      assert(get("#tag {a 1}", "[a]") === Right(read("1")))
      assert(get("[#tag a]", "[0]") === Right(read("#tag a")))
    }

    it("update/set") {
      assert(set("#tag {a 1}", "[a]", "2") === Right(read("#tag {a 2}")))
      assert(set("[#tag {a 1}]", "[0 a]", "2") === Right(read("[#tag {a 2}]")))
    }
  }

  describe("other") {

    val untraversable =
      List("true", "false", "\"txt\"", "\\a", "1", "2.5", "1/2", "symbol", ":keyword")

    untraversable.foreach { data =>
      describe(data) {
        it(s"get") {
          assert(errorContains(get(data, "[a]"), "Can't descend on value"))
        }

        it(s"update/set") {
          assert(errorContains(set(data, "[a]", "value"), "Can't descend on value"))
        }
      }
    }
  }
}
