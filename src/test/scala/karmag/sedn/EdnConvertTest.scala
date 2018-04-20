package karmag.sedn

import karmag.sedn.Edn._
import org.scalatest.FunSuite

class EdnConvertTest extends FunSuite
  with EdnConvert.Converters {

  test("Strict atom converters") {
    assert(ENil.asEither[Null] === Right(null))
    assert(EBool(true).asEither[Boolean] === Right(true))
    assert(EString("text").asEither[String] === Right("text"))
    assert(EChar('a').asEither[Character] === Right('a'))
    assert(EInt(10).asEither[Int] === Right(10))
    assert(EInt(10).asEither[Long] === Right(10))
    assert(EInt(10).asEither[BigInt] === Right(10))
    assert(EDecimal(1.0).asEither[Float] === Right(1.0))
    assert(EDecimal(1.0).asEither[Double] === Right(1.0))
    assert(EDecimal(1.0).asEither[BigDecimal] === Right(1.0))
    // ratio
    assert(ESymbol("abc").asEither[Symbol] === Right('abc))
    // keyword

    // null
    assert(true.toEdnEither === Right(EBool(true)))
    assert("txt".toEdnEither === Right(EString("txt")))
    assert('a'.toEdnEither === Right(EChar('a')))
    assert(10.toEdnEither === Right(EInt(10)))
    assert(10L.toEdnEither === Right(EInt(10)))
    assert(BigInt(10).toEdnEither === Right(EInt(10)))
    assert(1.0.toEdnEither === Right(EDecimal(1.0)))
    assert(1.0D.toEdnEither === Right(EDecimal(1.0)))
    assert(BigDecimal("1.0").toEdnEither === Right(EDecimal(1.0)))
    // ratio
    assert('sym.toEdnEither === Right(ESymbol("sym")))
    // keyword
  }

  test("Collection converters") {
    assert(EList(List(EInt(1))).asEither[List[Int]] === Right(List(1)))
    assert(EVector(Vector(EInt(1))).asEither[Vector[Int]] === Right(Vector(1)))
    assert(EMap(Map(EString("key") -> EInt(1))).asEither[Map[String, Int]] === Right(Map("key" -> 1)))
    assert(ESet(Set(EInt(1))).asEither[Set[Int]] === Right(Set(1)))

    assert(List(1).toEdnEither === Right(EList(List(EInt(1)))))
    assert(Vector(1).toEdnEither === Right(EVector(Vector(EInt(1)))))
    assert(Map("key" -> 1).toEdnEither === Right(EMap(Map(EString("key") -> EInt(1)))))
    assert(Set(1).toEdnEither === Right(ESet(Set(EInt(1)))))
  }
}
