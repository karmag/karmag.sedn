package karmag.sedn

import java.io.StringWriter

import org.scalatest.FunSuite

import scala.io.Source

class Example extends FunSuite {

  test("Example") {
    // This gives us access to the AST.
    import karmag.sedn.Edn._
    // This gives us access to pretty much all of the functionality for working
    // with EDN data.
    import karmag.sedn.Import.All._

    val edn: Edn = EMap(Map.empty) // example data

    //--------------------------------------------------------------------------
    // Creating data

    "hello".toEdn
    1.toEdn
    Map("a" -> true, "b" -> false).toEdn

    "keyword".kw
    "keyword".keyword

    EString("hello")
    ESet(Set("a", "b", "c"))
    ETag(ESymbol("tag"), "value")

    //--------------------------------------------------------------------------
    // Loading data

    EdnIo.read("hello").toStream
    EdnIo.read(Source.fromString("hello")).toStream

    //--------------------------------------------------------------------------
    // Writing data

    edn.prettyString()
    // edn.prettyPrint() // printing to std out
    edn.compactString()
    // edn.compactPrint() // printing to std out

    val writer = new StringWriter
    EdnIo.write(writer, edn)

    //--------------------------------------------------------------------------
    // Path - modifying data

    // Path functions similarly to clojure functions get-in, update-in, etc.
    val namePath = path("users".kw, "Alpha", "name".kw) // [:users "Alpha" :name]

    // There are a number of different ways to invoke methods on path/data.
    namePath.withData(edn).set("First Last")
    edn.at(namePath).set("First Last")
    edn.at("users".kw, "Alpha", "name".kw).set("First Last")

    // Fetch data
    edn.at(namePath).get()

    // Update data
    edn.at(namePath).update {
      case EString(name) => "Dr. " + name
      case _ => "Dr. First Last"
    }

    //--------------------------------------------------------------------------
    // Walking data

    // Sum the length of all strings.
    edn.inspect[Int](0, { case (EString(str), n) => str.length + n })

    // Transform all keywords to strings.
    edn.transform({ case EKeyword(kw) => EString(kw) })

    // Remove all tags from data.
    edn.transform { case ETag(_, value) => value }

    // Both inspection and traversal at the same time.
    edn.walk[Int](
      state = 0,
      pre = { case (ETag(_, value), n) => (value, n + 1) })

    //--------------------------------------------------------------------------
    // Schema

    import karmag.sedn.EdnSchemas._

    // Schema checks return ENil on success and an edn datastructure describing
    // the error on failure.

    // Each type has a corresponding schema.
    eNil.check(ENil)
    eInt.check(EInt(12))
    eMap.check(EMap(Map()))
    //...

    // Map validation that requires :name and :age. Validates :color if present
    // and allow any number of additional key values as long as keys are keywords.
    map(
      req("name".kw)  -> and(eString, not(empty)),
      req("age".kw)   -> and(eInt, pred[Int](n => 0 <= n && n <= 100).errorMessage("0 <= x <= 100")),
      opt("color".kw) -> enum("red", "green", "blue")
    ).allowMore(eKeyword, any)

    vector(eInt, eString) // Accepts [10, "hello"]
    vectorOf(eInt) // Accepts [10, 5, 20, ...]
  }
}
