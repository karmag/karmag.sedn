package karmag.sedn

import org.scalatest.FunSpec
import TestUtil.read
import karmag.sedn.Edn._

class EdnTraverseTest extends FunSpec {

  describe("walk") {
    it("basic") {
      assert(
        EdnTraverse.walk[Vector[Int]](
          read("[1 {10 100} #{1000}]"),
          Vector[Int](),
          pre = { case (EInt(i), v) => EInt(i + 1) -> (v :+ i.toInt) }
        ) === read("[2 {11 101} #{1001}]") -> Vector(1, 10, 100, 1000))
    }

    it("pre/post") {
      assert(
        EdnTraverse.walk[Vector[Int]](
          read("[1 5]"),
          Vector[Int](),
          { case (EInt(i), v) => EInt(i + 1) -> (v :+ i.toInt) },
          { case (EInt(i), v) => EInt(i * 10) -> (v :+ i.toInt) }
        ) === read("[20 60]") -> Vector(1, 2, 5, 6))
    }
  }

  describe("inspect") {
    it("basic") {
      assert(
        EdnTraverse.inspect[Int](read("[1 {10 100} #{1000}]"), 0,
          pre = { case (EInt(i), n) => i.toInt + n }
        ) === 1 + 10 + 100 + 1000)
    }

    it("pre/post") {
      assert(
        EdnTraverse.inspect[Int](read("[2 4]"), 0,
          pre = { case (EInt(i), n) => i.toInt + n },
          post = { case (EInt(i), n) => i.toInt * n }
        ) === ((2 * 2) + 4) * 4)
    }
  }

  describe("transform") {
    it("basic") {
      assert(
        EdnTraverse.transform(
          read("[1 {2 3} #{4} #tag 5]"),
          pre = { case EInt(i) => EInt(i * 10) }
        ) === read("[10 {20 30} #{40} #tag 50]"))
    }

    it("pre/post") {
      assert(
        EdnTraverse.transform(
          read("[1 {2 3} #{4} #tag 5]"),
          pre = { case EInt(i) => EInt(i * 10) },
          post = { case EInt(i) => EString(i.toString()) }
        ) === read("""["10" {"20" "30"} #{"40"} #tag "50"]"""))
    }
  }
}
