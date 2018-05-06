package karmag.sedn

import java.io._

import clojure.java.api.Clojure
import clojure.lang.Named
import karmag.sedn.Clj.Tagged
import karmag.sedn.Edn._

import scala.collection.immutable.Stream.cons
import scala.io.Source
import scala.util.{Failure, Success, Try}

object EdnIo {

  type EdnReader = () => Option[Try[Edn]]

  object Implicits extends EdnIoMethods

  trait EdnIoMethods {
    implicit class RichEdnReader(ednReader: EdnReader) {
      def toStream: Stream[Edn] = EdnIo.toStream(ednReader)
    }

    implicit class RichEdnIo(edn: Edn) {
      def prettyPrint(): Unit = EdnIo.prettyPrint(edn)
      def prettyString(): String = EdnIo.prettyString(edn)
      def compactPrint(): Unit = println(EdnIo.compactString(edn))
      def compactString(): String = {
        val writer = new StringWriter
        EdnIo.write(writer, edn)
        writer.toString
      }
    }
  }

  private lazy val push = Clojure.`var`("clojure.core", "push-thread-bindings")
  private lazy val pop = Clojure.`var`("clojure.core", "pop-thread-bindings")
  private lazy val out = Clojure.`var`("clojure.core", "*out*")
  private lazy val pr = Clojure.`var`("clojure.core", "pr")
  private lazy val pprint = {
    val require = Clojure.`var`("clojure.core", "require")
    require.invoke(clojure.lang.Symbol.intern("clojure.pprint"))
    Clojure.`var`("clojure.pprint", "pprint")
  }

  def read(source: Source): EdnReader = read(from(source))

  def read(reader: Reader): EdnReader = {
    val pbr = from(reader)

    () => {
      Try(Clj.readEdn(pbr)) match {
        case Success(item) =>
          if (item == Clj.EOF) {
            pbr.close()
            None
          } else
            Option(
              ObjectToEdnTranslation.translate(item) match {
                case Right(edn) =>
                  Success(edn)
                case Left(error) =>
                  pbr.close()
                  Failure(new Exception(error))
              }
            )
        case Failure(error) => Option(Failure(error))
      }
    }
  }

  def read(string: String): EdnReader = read(new StringReader(string))

  def write(writer: Writer, edn: Edn): Unit = {
    push.invoke(clojure.lang.PersistentHashMap.create(out, writer))
    try {
      write(writer, pr.invoke(_), edn)
      writer.flush()
    } finally {
      pop.invoke()
    }
  }

  private def write(writer: Writer, pr: Any => Unit, edn: Edn): Unit = {
    edn match {
      case ENil | EBool(_) | EString(_) | EChar(_) | EInt(_) | EDecimal(_) | ERatio(_, _) | ESymbol(_) | EKeyword(_) =>
        pr(EdnToObjectTranslation.translate(edn))
      case EList(list) =>
        writeCollection(writer, pr, list, "(", ")")
      case EVector(vector) =>
        writeCollection(writer, pr, vector, "[", "]")
      case EMap(map) =>
        var first = true
        writer.write("{")
        map.foreach { case (key, value) =>
          if (first) {
            first = false
            write(writer, pr, key)
            writer.write(" ")
            write(writer, pr, value)
          } else {
            writer.write(", ")
            write(writer, pr, key)
            writer.write(" ")
            write(writer, pr, value)
          }
        }
        writer.write("}")
      case ESet(set) =>
        writeCollection(writer, pr, set, "#{", "}")
      case ETag(tag, value) =>
        writer.write("#")
        write(writer, pr, tag)
        writer.write(" ")
        write(writer, pr, value)
    }
  }

  private def writeCollection(writer: Writer,
                              pr: Any => Unit,
                              items: Iterable[Edn],
                              start: String,
                              stop: String): Unit = {
    var first = true
    writer.write(start)
    items.foreach { item =>
      if (first) {
        first = false
        write(writer, pr, item)
      } else {
        writer.write(" ")
        write(writer, pr, item)
      }
    }
    writer.write(stop)
  }

  def prettyPrint(writer: Writer, edn: Edn): Unit =
    pprint.invoke(EdnToObjectTranslation.translate(edn), writer)

  def prettyPrint(edn: Edn): Unit = println(prettyString(edn))

  def prettyString(edn: Edn): String = {
    val sw = new StringWriter()
    prettyPrint(sw, edn)
    sw.toString.trim
  }

  def compactString(edn: Edn): String = {
    val sw = new StringWriter()
    write(sw, edn)
    sw.toString.trim
  }

  private def toStream(ednReader: EdnReader): Stream[Edn] =
    ednReader() match {
      case Some(data) => data match {
        case Success(edn) => cons(edn, toStream(ednReader))
        case Failure(error) => throw error
      }
      case None => Stream.Empty
    }

  private def from(source: Source): PushbackReader =
    from(
      new StringReader(source.mkString)
//      new Reader {
//        override def read(cbuf: Array[Char], off: Int, len: Int): Int = {
//          var counter = 0
//          while (counter < len && source.hasNext) {
//            cbuf(off + counter) = source.next()
//            counter += 1
//          }
//          counter
//        }
//
//        override def close(): Unit = source.close()
//      }
    )

  private def from(reader: Reader): PushbackReader =
    reader match {
      case _: PushbackReader => reader.asInstanceOf[PushbackReader]
      case _ => new PushbackReader(reader)
    }
}

private object Clj {

  val EOF = new Object

  private val TaggedReader = new TaggedReader
  private val UuidReader = new BuiltinReader(symbol("uuid"))
  private val InstReader = new BuiltinReader(symbol("inst"))

  case class Tagged(tag: clojure.lang.Symbol, value: Object)

  private class TaggedReader extends clojure.lang.AFn {
    override def invoke(tag: Object, value: Object): Object =
      Tagged(tag.asInstanceOf[clojure.lang.Symbol], value)
  }

  private class BuiltinReader(val tag: clojure.lang.Symbol) extends clojure.lang.AFn {
    override def invoke(value: Object): Object = Tagged(tag, value)
  }

  def readEdn(in: PushbackReader): Object = {
    val readers = new java.util.HashMap[Object, Object]()
    readers.put(symbol("uuid"), UuidReader)
    readers.put(symbol("inst"), InstReader)

    val options = new java.util.HashMap[Object, Object]()
    options.put(keyword("eof"), EOF)
    options.put(keyword("readers"), map(readers))
    options.put(keyword("default"), TaggedReader)

    val reader = new clojure.edn$read
    reader.invoke(map(options), in)
  }

  private def map(data: java.util.HashMap[Object, Object]): clojure.lang.IPersistentMap =
    clojure.lang.PersistentArrayMap.create(data)

  private def keyword(str: String): clojure.lang.Keyword = clojure.lang.Keyword.intern(str)

  private def symbol(str: String): clojure.lang.Symbol = clojure.lang.Symbol.intern(str)
}

private object EdnToObjectTranslation {
  import scala.collection.JavaConverters._

  def translate(edn: Edn): Object = {
    edn match {
      case ENil         => null
      case EBool(b)     => Boolean.box(b)
      case EString(s)   => s
      case EChar(c)     => c
      case EInt(i)      => i
      case EDecimal(d)  => d
      case ERatio(n, d) => new clojure.lang.Ratio(n.bigInteger, d.bigInteger)
      case ESymbol(sym) => clojure.lang.Symbol.intern(sym)
      case EKeyword(kw) => clojure.lang.Keyword.intern(kw)
      case EList(list)  => clojure.lang.PersistentList.create(list.map(translate).asJava)
      case EVector(vec) => clojure.lang.PersistentVector.create(vec.map(translate).asJava)
      case EMap(map)    => clojure.lang.PersistentHashMap.create(map.map(kv => translate(kv._1) -> translate(kv._2)).asJava)
      case ESet(set)    => clojure.lang.PersistentHashSet.create(set.map(translate).toList.asJava)
      case ETag(tag, value) => translate(value) // TODO karl: how to deal with tags?
    }
  }
}

private object ObjectToEdnTranslation {

  private val get = Clojure.`var`("clojure.core", "get")
  private val notEmpty = Clojure.`var`("clojure.core", "not-empty")

  def translate(obj: Object): Either[String, Edn] =
    obj match {
      case null => Right(ENil)
      case value: java.lang.Boolean => Right(EBool(value))
      case value: String => Right(EString(value))
      case value: Character => Right(EChar(value))
      case value: java.lang.Long => Right(EInt(value.toLong))
      case value: clojure.lang.BigInt => Right(EInt(value.toBigInteger))
      case value: java.lang.Double => Right(EDecimal(value.toDouble))
      case value: java.math.BigDecimal => Right(EDecimal(value))
      case value: clojure.lang.Ratio => Right(ERatio(value.numerator, value.denominator))
      case value: clojure.lang.Symbol => Right(ESymbol(fromNamed(value)))
      case value: clojure.lang.Keyword => Right(EKeyword(fromNamed(value)))
      case value: clojure.lang.IPersistentList => Right(toList(value))
      case value: clojure.lang.IPersistentVector => Right(toVector(value))
      case value: clojure.lang.IPersistentMap => Right(toMap(value))
      case value: clojure.lang.IPersistentSet => Right(toSet(value))
      case value: Tagged => translate(value.value) match {
        case Right(x) => Right(ETag(ESymbol(fromNamed(value.tag)), x))
        case Left(e) => Left(e)
      }
      case _ => Left("Can't translate type: " + obj.getClass)
    }

  private def _translate(obj: Object): Edn =
    translate(obj) match {
      case Right(value) => value
      case Left(error) => throw new RuntimeException(error)
    }

  private def fromNamed(named: Named): String =
    if (named.getNamespace == null)
      named.getName
    else
      named.getNamespace + "/" + named.getName

  private def toList(obj: clojure.lang.IPersistentList): EList = {
    var items = obj.asInstanceOf[clojure.lang.ISeq]
    var data: List[Edn] = List[Edn]()

    while (notEmpty.invoke(items) != null) {
      val item = items.first()
      data = data :+ _translate(item)
      items = items.next()
    }

    EList(data)
  }

  private def toVector(obj: clojure.lang.IPersistentVector): EVector = {
    var items = obj.asInstanceOf[clojure.lang.IPersistentStack]
    var vec: Vector[Edn] = Vector[Edn]()

    while (notEmpty.invoke(items) != null) {
      val item = items.peek()
      vec = _translate(item) +: vec
      items = items.pop()
    }

    EVector(vec)
  }

  private def toMap(obj: clojure.lang.IPersistentMap): EMap = {
    var keys = new clojure.core$keys().invoke(obj).asInstanceOf[clojure.lang.ISeq]
    var data: Map[Edn, Edn] = Map[Edn, Edn]()

    while (notEmpty.invoke(keys) != null) {
      val key = keys.first()
      data = data + (_translate(key) -> _translate(get.invoke(obj, key)))
      keys = keys.next()
    }
    EMap(data)
  }

  private def toSet(obj: clojure.lang.IPersistentSet): ESet = {
    var items = obj.seq()
    var data: Set[Edn] = Set[Edn]()

    while (notEmpty.invoke(items) != null) {
      val item = items.first()
      data += _translate(item)
      items = items.next()
    }

    ESet(data)
  }
}
