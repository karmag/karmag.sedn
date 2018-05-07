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
      def prettyPrint(width: Int = 80, indent: String = "  "): Unit =
        EdnIo.prettyPrint(edn, width, indent)
      def prettyString(width: Int = 80, indent: String = "  "): String =
        EdnIo.prettyString(edn, width, indent)
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

  /** Writes EDN data using indents and line break for readability. width is a
    * hint for when to use line breaks but is not a hard limit. indent is the
    * string used for indenting the data. */
  def writePretty(writer: Writer, edn: Edn, width: Int = 80, indent: String = "  "): Unit =
    Pretty.write(writer, edn, Pretty.Config(width = width, indentString = indent))

  def prettyPrint(edn: Edn, width: Int = 80, indent: String = "  "): Unit =
    println(prettyString(edn, width, indent))

  def prettyString(edn: Edn, width: Int = 80, indent: String = "  "): String = {
    val sw = new StringWriter()
    writePretty(sw, edn, width, indent)
    sw.toString.trim
  }

  def compactString(edn: Edn): String = {
    val sw = new StringWriter()
    write(sw, edn)
    sw.toString.trim
  }

  def compactPrint(edn: Edn): Unit = println(compactString(edn))

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

private object Pretty {

  case class Config(width: Int = 80,
                    indentLevel: Int = 0,
                    indentString: String = "  ") {
    def inc: Config = copy(indentLevel = indentLevel + 1)
  }

  def write(writer: Writer, edn: Edn, config: Config = Config()): Unit =
    if (estimateSize(edn) > config.width)
      writePrettyItem(writer, edn, config)
    else {
      writeIndent(writer, config)
      EdnIo.write(writer, edn)
      writer.write("\n")
    }

  private def writePrettyItem(writer: Writer,
                              edn: Edn,
                              config: Config,
                              leadIndent: Boolean = true): Unit = {
    edn match {
      case ENil | EBool(_) | EString(_) | EChar(_) | EInt(_) | EDecimal(_) | ERatio(_, _) | ESymbol(_) | EKeyword(_) =>
        if (leadIndent)
          writeIndent(writer, config)
        EdnIo.write(writer, edn)
        writer.write("\n")
      case EList(list) =>
        writePrettyCollection(writer, list, config, "(", ")", leadIndent)
      case EVector(vector) =>
        writePrettyCollection(writer, vector, config, "[", "]", leadIndent)
      case EMap(map) =>
        writePrettyMap(writer, map, config, leadIndent)
      case ESet(set) =>
        writePrettyCollection(writer, set, config, "#{", "}", leadIndent)
      case ETag(tag, value) =>
        if (leadIndent)
          writeIndent(writer, config)
        writer.write("#")
        EdnIo.write(writer, tag)
        writer.write(" ")
        writePrettyItem(writer, value, config.inc, leadIndent = false)
    }
  }

  private def writePrettyCollection(writer: Writer,
                                    items: Iterable[Edn],
                                    config: Config,
                                    start: String,
                                    stop: String,
                                    leadIndent: Boolean = true): Unit = {
    if (leadIndent)
      writeIndent(writer, config)
    writer.write(start)
    if (items.isEmpty) {
      writer.write(stop)
      writer.write("\n")
    } else {
      val incConfig = config.inc
      writer.write("\n")
      items.foreach { item =>
        write(writer, item, incConfig)
      }
      writeIndent(writer, config)
      writer.write(stop)
      writer.write("\n")
    }
  }

  private def writePrettyMap(writer: Writer,
                             map: Map[Edn, Edn],
                             config: Config,
                             leadIndent: Boolean = true): Unit = {
    if (leadIndent)
      writeIndent(writer, config)
    writer.write("{")
    if (map.isEmpty) {
      writer.write("}")
      writer.write("\n")
    } else {
      val incConfig = config.inc
      writer.write("\n")
      map.foreach { case (key, value) =>
        val keySize = estimateSize(key)
        val valSize = estimateSize(value)
        if (keySize > config.width || valSize > config.width) {
          if (keySize > config.width) {
            writePrettyItem(writer, key, incConfig)
            if (valSize > config.width) {
              writePrettyItem(writer, value, incConfig)
            } else {
              writeIndent(writer, incConfig.inc)
              EdnIo.write(writer, value)
              writer.write("\n")
            }
          } else {
            writeIndent(writer, incConfig)
            EdnIo.write(writer, key)
            writer.write("\n")
            writePrettyItem(writer, value, incConfig.inc)
          }
        } else {
          writeIndent(writer, incConfig)
          EdnIo.write(writer, key)
          writer.write(" ")
          EdnIo.write(writer, value)
          writer.write("\n")
        }
      }
      writeIndent(writer, config)
      writer.write("}")
      writer.write("\n")
    }
  }

  private def writeIndent(writer: Writer, config: Config): Unit =
    writer.write(config.indentString * config.indentLevel)

  // Estimated length of the single line string rendering the edn data.
  private def estimateSize(edn: Edn): Int = {
    edn match {
      case ENil             => 3
      case EBool(_)         => 5
      case EString(value)   => value.length() + 2
      case EChar(_)         => 2
      case EInt(value)      => value.toString().length
      case EDecimal(value)  => value.toString().length
      case ERatio(n, d)     => n.toString().length + d.toString().length
      case ESymbol(value)   => value.length
      case EKeyword(value)  => value.length + 1
      case EList(list)      => list.foldLeft(0)(_ + estimateSize(_)) + 2 + list.size
      case EVector(vector)  => vector.foldLeft(0)(_ + estimateSize(_)) + 2 + vector.size
      case EMap(map)        => map.foldLeft(0) { case (n, (k, v)) => n + estimateSize(k) + estimateSize(v) } + 2 + map.size * 2
      case ESet(set)        => set.foldLeft(0)(_ + estimateSize(_)) + 2 + set.size
      case ETag(tag, value) => estimateSize(tag) + estimateSize(value) + 2
    }
  }
}
