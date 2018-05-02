package karmag.sedn

import org.scalatest.FunSpec

class EdnMergeTest extends FunSpec {

  private val testData =
    List(
      ("map",
        List(
          ("{:a 1}",                "{:b 2}",        "{:a 1 :b 2}"),
          ("{:a {:b 100}}",         "{:a {:b 200}}", "{:a {:b 200}}"),
          ("{:a 1}",                "{:a nil}",      "{}"),
          ("{:a {:b 100, :c 200}}", "{:a {:b nil}}", "{:a {:c 200}}"),
          ("{:a 1}",                ":value",        ":value"))),
      ("set",
        List(
          ("#{:a :b}", "#{:c}",            "#{:a :b :c}"),
          ("#{:a :b}", "{:b nil}",         "#{:a}"),
          ("#{:a :b}", "{:b false}",       "#{:a :b}"),
          ("#{:a}",    "{:b true :c :go}", "#{:a :b :c}"),
          ("#{:a}",    "{:a nil}",         "#{}"),
          ("#{:a}",    ":value",           ":value"))),
      ("vector",
        List(
          ("[:a :b]", "{0 :c}",   "[:c :b]"),
          ("[:a]",    "{3 :c}",   "[:a nil nil :c]"),
          ("[:a :b]", "[:c]",     "[:c]"),
          ("[:a :b]", ":value",   ":value"))),
      ("combined",
        List(
          ("{:key #{:a}}",       "{:key {:a nil :b :add}}",         "{:key #{:b}}"),
          ("[{:key [1]}]",       "{0 {:key {0 101}}}",              "[{:key [101]}]"),
          ("[{:key [1 #{:x}]}]", "{0 {:key {1 {:x nil :a true}}}}", "[{:key [1 #{:a}]}]")))
    )

  testData.foreach { case (groupName, tests) =>
    describe(groupName) {

      tests.foreach { case (inputText, patchText, expectedText) =>
        it(s"$inputText + $patchText") {
          val input = TestUtil.read(inputText)
          val patch = TestUtil.read(patchText)
          val expected = TestUtil.read(expectedText)

          val actual = EdnMerge.merge(input, patch)
          assert(expected === actual)
        }
      }
    }
  }

  describe("merge multiple") {
    import EdnMerge.Implicits._

    val a = TestUtil.read("""{:a 1, :b {:q #{1}}}""")
    val b = TestUtil.read("""{:a 2, :b {:q {1 nil}}}""")
    val c = TestUtil.read("""{:a 3, :b {:q {10 true}}}""")
    val result = a.merge(b, c)

    assert(result === TestUtil.read("""{:a 3, :b {:q #{10}}}"""))
  }
}
