package karmag.sedn

import karmag.sedn.Edn._
import karmag.sedn.EdnSchema.Schema
import karmag.sedn.EdnSchemas._
import org.scalatest.FunSpec

class EdnSchemaTest extends FunSpec {

  private val Pass = ENil

  private def isFailure(edn: Edn): Boolean = edn != Pass

  private def stuff: List[Edn] = List(
    "nil", "true", "\"text\"", "\\g", "101", "1.5",
    "1/7", "symbol", ":keyword", "(1 2 3)", "[1 2 3]",
    "{:a 1}", "#{1 2 3}", "#tag 1").map(TestUtil.read)

  describe("edn type schemas") {
    val pairs = List(
      (eNil,     "nil"),
      (eBool,    "true"),
      (eString,  "\"text\""),
      (eChar,    "\\g"),
      (eInt,     "101"),
      (eDecimal, "1.5"),
      (eRatio,   "1/7"),
      (eSymbol,  "symbol"),
      (eKeyword, ":keyword"),
      (eList,    "(1 2 3)"),
      (eVector,  "[1 2 3]"),
      (eMap,     "{:a 1}"),
      (eSet,     "#{1 2 3}"),
      (eTag,     "#tag 1")).map { case (schema, txt) => schema -> TestUtil.read(txt) }

    pairs.foreach { case (schema, matchingValue) =>

      describe(matchingValue.getClass.getSimpleName) {
        it("matching") {
          assert(schema.check(matchingValue) === Pass)
        }

        pairs.map(_._2).filter(_ != matchingValue).foreach { nonMatchingValue =>
          it(s"non matching - $nonMatchingValue") {
            assert(isFailure(schema.check(nonMatchingValue)))
          }
        }
      }
    }
  }

  describe("any") {
    stuff.foreach { edn =>
      it(EdnIo.compactString(edn)) {
        assert(any.check(edn) === Pass)
      }
    }
  }

  describe("none") {
    stuff.foreach { edn =>
      it(EdnIo.compactString(edn)) {
        assert(isFailure(none.check(edn)))
      }
    }
  }

  it("is") {
    val schema = is(EString("hello"))

    assert(schema.check(EString("hello")) === Pass)
    assert(isFailure(schema.check(EString("bye"))))
    assert(isFailure(schema.check(EBool(true))))
  }

  describe("enum") {
    import EdnConvert.Implicits._
    val schema = enum("a", "b", "c")

    it("ok") {
      assert(schema.check("a") === Pass)
      assert(schema.check("c") === Pass)
    }

    it("failure") {
      assert(isFailure(schema.check("d")))
      assert(isFailure(schema.check(10)))
    }
  }

  describe("and") {
    val schema = and(eInt, range(1, 5))

    it("ok") {
      val values = List("1", "5").map(TestUtil.read)
      assert(values.forall(schema.check(_) == Pass))
    }

    it("failure") {
      val values = List("0", "6", "\"hello\"", "[1 2 3]").map(TestUtil.read)
      assert(values.forall(x => isFailure(schema.check(x))))
    }
  }

  describe("or") {
    val schema = or(eInt, range(1, 5))

    it("ok") {
      val values = List("1", "5", "101010", "\"hello\"", "[1 2 3]").map(TestUtil.read)
      assert(values.forall(schema.check(_) == Pass))
    }

    it("failure") {
      val values = List("\"123456\"", "[]").map(TestUtil.read)
      assert(values.forall(x => isFailure(schema.check(x))))
    }
  }

  describe("fail") {
    val schema = EdnSchemas.fail("Boom!")
    stuff.foreach { edn =>
      it(EdnIo.compactString(edn)) {
        assert(isFailure(schema.check(edn)))
      }
    }
  }

  describe("tag") {
    val schema = tag("abc", eInt)

    it("ok") {
      assert(schema.check(TestUtil.read("""#abc 10""")) === Pass)
    }

    it("failure") {
      assert(isFailure(schema.check(TestUtil.read("""#qwe 10"""))))
      assert(isFailure(schema.check(TestUtil.read("""#abc :key"""))))
    }
  }

  describe("seq") {
    val schema = seq(eInt, eString, eNil)

    it("ok") {
      assert(schema.check(TestUtil.read("""(1, "hello", nil)""")) === Pass)
      assert(schema.check(TestUtil.read("""[1, "hello", nil]""")) === Pass)
    }

    it("failure") {
      assert(isFailure(schema.check(TestUtil.read("""#{1, "hello", nil}"""))))
      assert(isFailure(schema.check(TestUtil.read("""[1, "hello"]"""))))
      assert(isFailure(schema.check(TestUtil.read("""[1, "hello", "text"]"""))))
      assert(isFailure(schema.check(TestUtil.read("""[1, "hello", nil, 2]"""))))
    }
  }

  describe("seq of") {
    val schema = seqOf(eInt)

    it("ok") {
      assert(schema.check(TestUtil.read("""(1 2 3)""")) === Pass)
      assert(schema.check(TestUtil.read("""[1 2 3]""")) === Pass)
      assert(schema.check(TestUtil.read("""#{1 2 3}""")) === Pass)
      assert(schema.check(TestUtil.read("""[]""")) === Pass)
    }

    it("failure") {
      assert(isFailure(schema.check(TestUtil.read("""[1 "txt"]"""))))
      assert(isFailure(schema.check(TestUtil.read("""{2 1}"""))))
    }
  }

  describe("map") {
    import EdnConvert.Implicits._

    val schema = map(req("req".kw) -> eBool, opt("opt".kw) -> eInt)

    describe("required key") {
      it("ok") {
        assert(schema.check(TestUtil.read("""{:req true, :opt 10}""")) === Pass)
      }

      it("missing required key") {
        val result = schema.check(TestUtil.read("""{:opt 10}"""))
        assert(isFailure(result))
      }
    }

    describe("optional key") {
      it("ok") {
        assert(schema.check(TestUtil.read("""{:req true, :opt 10}""")) === Pass)
      }

      it("missing optional key") {
        assert(schema.check(TestUtil.read("""{:req true}""")) === Pass)
      }
    }

    describe("additional keys") {
      val schema2 = schema.allowMore(eKeyword, eString)

      it("ok") {
        assert(schema2.check(TestUtil.read("""{:req true, :more "yes"}""")) === Pass)
      }

      it("validation failure") {
        assert(isFailure(schema2.check(TestUtil.read("""{:req true, "key" "yes"}"""))))
        assert(isFailure(schema2.check(TestUtil.read("""{:req true, :more 10}"""))))
      }
    }

    describe("merge") {

      describe("keys") {
        val schema =
          map(req("alpha".kw) -> eString, req("omega".kw) -> eString)
            .merge(map(req("beta".kw) -> eInt, req("omega".kw) -> eInt))

        it("ok") {
          assert(schema.check(TestUtil.read("""{:alpha "s", :beta 1, :omega 2}""")) === Pass)
        }

        it("failure") {
          assert(isFailure(schema.check(TestUtil.read("""{:alpha "s", :beta 1, :omega "text"}"""))))
        }
      }

      describe("more") {
        val schema1 = map().allowMore(eKeyword, eInt)
        val schema2 = map().allowMore(eString, eBool)

        it("ok") {
          assert(schema1.merge(schema2).check(TestUtil.read("""{"s" true}""")) == Pass)
          assert(schema2.merge(schema1).check(TestUtil.read("""{:k 1}""")) == Pass)
        }

        it("failure") {
          assert(isFailure(schema1.merge(schema2).check(TestUtil.read("""{:k 1}"""))))
          assert(isFailure(schema2.merge(schema1).check(TestUtil.read("""{"s" true}"""))))
        }
      }
    }
  }

  describe("mapOf") {
    val schema = mapOf(eInt, eKeyword)

    it("ok") {
      assert(schema.check(TestUtil.read("""{1 :a, 2 :b}""")) === Pass)
      assert(schema.check(TestUtil.read("""{}""")) === Pass)
    }

    it("failure") {
      assert(isFailure(schema.check(TestUtil.read("""{1 :a, 2 "tre"}"""))))
      assert(isFailure(schema.check(TestUtil.read("""#{}"""))))
    }
  }

  describe("pred") {
    import EdnConvert.Implicits._

    val schema = pred[String](_.length > 2)

    it("ok") {
      assert(schema.check("123") === Pass)
    }

    it("failure") {
      assert(isFailure(schema.check(1)))
      assert(isFailure(schema.check("1")))
      assert(isFailure(schema.check(TestUtil.read("[1 2 3]"))))
    }
  }

  describe("edn pred") {
    val schema = ednPred { case EString("hello") => true }

    it("ok") {
      assert(schema.check(TestUtil.read("\"hello\"")) === Pass)
    }

    it("failure") {
      assert(isFailure(schema.check(TestUtil.read("\"bye\""))))
      assert(isFailure(schema.check(TestUtil.read("10"))))
    }
  }

  describe("range") {
    val schema = range(1, 5)

    describe("ok") {
      val values = List(
        "\"abc\"",
        "3",
        "3.5",
        "3/2",
        "(1 2 3)",
        "[1 2 3]",
        "#{1 2 3}",
        "{:a 1, :b 2}").map(TestUtil.read)

      values.foreach { value =>
        it(EdnIo.compactString(value)) {
          assert(schema.check(value) === Pass)
        }
      }
    }

    describe("failure") {
      val values = List(
        "\"\"", "\"abcdef\"",
        "0", "10",
        "0.9", "5.1",
        "1/2", "51/10",
        "()", "(1 2 3 4 5 6)",
        "[]", "[1 2 3 4 5 6]",
        "#{}", "#{1 2 3 4 5 6}",
        "{}", "{a 1 b 2 c 3 d 4 e 5 f 6}").map(TestUtil.read)

      values.foreach { value =>
        it(EdnIo.compactString(value)) {
          assert(isFailure(schema.check(value)))
        }
      }
    }
  }

  describe("empty") {
    it("ok") {
      assert(empty.check(TestUtil.read("()")) === Pass)
      assert(empty.check(TestUtil.read("[]")) === Pass)
      assert(empty.check(TestUtil.read("#{}")) === Pass)
      assert(empty.check(TestUtil.read("{}")) === Pass)
      assert(empty.check(TestUtil.read("\"\"")) === Pass)
      assert(empty.check(TestUtil.read("nil")) === Pass)
    }

    it("failure") {
      assert(isFailure(empty.check(TestUtil.read("[1]"))))
      assert(isFailure(empty.check(TestUtil.read("\"a\""))))
      assert(isFailure(empty.check(TestUtil.read("4"))))
      assert(isFailure(empty.check(TestUtil.read("true"))))
    }
  }

  describe("not") {
    val schema = not(eInt)

    it("ok") {
      assert(schema.check(TestUtil.read("\"abc\"")) === Pass)
      assert(schema.check(TestUtil.read("[]")) === Pass)
    }

    it("failure") {
      assert(isFailure(schema.check(TestUtil.read("1"))))
    }
  }

  describe("conditional") {
    import karmag.sedn.EdnPath._
    import karmag.sedn.EdnPath.Implicits._
    import karmag.sedn.EdnConvert.Implicits._

    val schema = conditional {
      case _: EString => Option(eString)
      case m: EMap =>
        path(EKeyword("type")).withData(m).get() match {
          case EString("int") =>
            Option(map(
              req("type".kw) -> is("int"),
              req("value".kw) -> eInt))
          case EString("bool") =>
            Option(map(
              req("type".kw) -> is("bool"),
              req("value".kw) -> eBool))
          case _ => None
        }
      case _ => None
    }

    it("ok") {
      assert(schema.check(TestUtil.read(""""text"""")) === Pass)
      assert(schema.check(TestUtil.read("""{:type "int" :value 10}""")) === Pass)
      assert(schema.check(TestUtil.read("""{:type "bool" :value true}""")) === Pass)
    }

    it("failure") {
      assert(isFailure(schema.check(TestUtil.read("10"))))
      assert(isFailure(schema.check(TestUtil.read("""{:type "int" :value true}"""))))
      assert(isFailure(schema.check(TestUtil.read("""{:type "x" :value true}"""))))
    }
  }
}
