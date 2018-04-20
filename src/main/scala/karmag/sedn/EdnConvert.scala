package karmag.sedn

import karmag.sedn.Edn._

import scala.collection.mutable.ListBuffer

object EdnConvert {

  object Implicits extends Base with Converters

  trait Base {
    trait EdnReader[T] { def read(edn: Edn): Either[String, T] }
    trait EdnWriter[T] { def write(obj: T): Either[String, Edn] }

    object EdnReader {
      implicit def fromFunction[T](f: (Edn) => Either[String, T]): EdnReader[T] =
        new EdnReader[T] { override def read(edn: Edn): Either[String, T] = f(edn) }
    }

    object EdnWriter {
      implicit def fromFunction[T](f: (T) => Either[String, Edn]): EdnWriter[T] =
        new EdnWriter[T] { override def write(obj: T): Either[String, Edn] = f(obj) }
    }

    implicit def autoWrite[T](x: T)(implicit writer: EdnWriter[T]): Edn = x.toEdn

    implicit class RichKeywordString(s: String) {
      def kw: EKeyword = EKeyword(s)
      def keyword: EKeyword = EKeyword(s)
    }

    implicit class EdnAs(edn: Edn) {
      def as[T](implicit reader: EdnReader[T]): T = asEither.fold(error => throw new Exception(error), identity)
      def asOr[T](default: T)(implicit reader: EdnReader[T]): T = asOpt.getOrElse(default)
      def asOpt[T](implicit reader: EdnReader[T]): Option[T] = asEither match {
        case Right(x) => Some(x)
        case Left(_) => None
      }
      def asEither[T](implicit reader: EdnReader[T]): Either[String, T] = reader.read(edn)
    }

    implicit class EdnWrite[T](any: T)(implicit writer: EdnWriter[T]) {
      def toEdn(implicit writer: EdnWriter[T]): Edn = toEdnEither.fold(error => throw new Exception(error), identity)
      def toEdnOr(default: Edn)(implicit writer: EdnWriter[T]): Edn = toEdnEither.fold(Function.const(default), identity)
      def toEdnOpt(implicit writer: EdnWriter[T]): Option[Edn] = toEdnEither match {
        case Right(x) => Some(x)
        case Left(_) => None
      }
      def toEdnEither(implicit writer: EdnWriter[T]): Either[String, Edn] = writer.write(any)
    }
  }

  trait Converters extends Base {
    private def readTypeError[T](expected: String, actual: Edn): Either[String, T] =
      Left(s"Expected $expected but got $actual")

    implicit object NullConverter extends EdnReader[Null] with EdnWriter[Null] {
      override def write(obj: Null): Either[String, Edn] = Right(ENil)

      override def read(edn: Edn): Either[String, Null] = edn match {
        case ENil => Right(null)
        case _ => readTypeError("null", edn)
      }
    }

    implicit object BoolConverter extends EdnReader[Boolean] with EdnWriter[Boolean] {
      override def write(obj: Boolean): Either[String, Edn] = Right(EBool(obj))

      override def read(edn: Edn): Either[String, Boolean] = edn match {
        case EBool(value) => Right(value)
        case _ => readTypeError("Boolean", edn)
      }
    }

    implicit object StringConverter extends EdnReader[String] with EdnWriter[String] {
      override def write(obj: String): Either[String, Edn] = Right(EString(obj))

      override def read(edn: Edn): Either[String, String] = edn match {
        case EString(value) => Right(value)
        case _ => readTypeError("String", edn)
      }
    }

    implicit object CharConverter extends EdnReader[Char] with EdnWriter[Char] {
      override def write(obj: Char): Either[String, Edn] = Right(EChar(obj))

      override def read(edn: Edn): Either[String, Char] = edn match {
        case EChar(value) => Right(value)
        case _ => readTypeError("Char", edn)
      }
    }

    implicit object CharacterConverter extends EdnReader[Character] with EdnWriter[Character] {
      override def write(obj: Character): Either[String, Edn] = Right(EChar(obj))

      override def read(edn: Edn): Either[String, Character] = edn match {
        case EChar(value) => Right(value)
        case _ => readTypeError("Character", edn)
      }
    }

    implicit object IntConverter extends EdnReader[Int] with EdnWriter[Int] {
      override def write(obj: Int): Either[String, Edn] = Right(EInt(obj))

      override def read(edn: Edn): Either[String, Int] = edn match {
        case EInt(value) => Right(value.intValue())
        case _ => readTypeError("Int", edn)
      }
    }

    implicit object LongConverter extends EdnReader[Long] with EdnWriter[Long] {
      override def write(obj: Long): Either[String, Edn] = Right(EInt(obj))

      override def read(edn: Edn): Either[String, Long] = edn match {
        case EInt(value) => Right(value.longValue())
        case _ => readTypeError("Long", edn)
      }
    }

    implicit object BigIntConverter extends EdnReader[BigInt] with EdnWriter[BigInt] {
      override def write(obj: BigInt): Either[String, Edn] = Right(EInt(obj))

      override def read(edn: Edn): Either[String, BigInt] = edn match {
        case EInt(value) => Right(value)
        case _ => readTypeError("BigInt", edn)
      }
    }

    implicit object FloatConverter extends EdnReader[Float] with EdnWriter[Float] {
      override def write(obj: Float): Either[String, Edn] = Right(EDecimal(obj.toDouble))

      override def read(edn: Edn): Either[String, Float] = edn match {
        case EDecimal(value) => Right(value.floatValue())
        case _ => readTypeError("Float", edn)
      }
    }

    implicit object DoubleConverter extends EdnReader[Double] with EdnWriter[Double] {
      override def write(obj: Double): Either[String, Edn] = Right(EDecimal(obj))

      override def read(edn: Edn): Either[String, Double] = edn match {
        case EDecimal(value) => Right(value.doubleValue())
        case _ => readTypeError("Double", edn)
      }
    }

    implicit object BigDecimalConverter extends EdnReader[BigDecimal] with EdnWriter[BigDecimal] {
      override def write(obj: BigDecimal): Either[String, Edn] = Right(EDecimal(obj))

      override def read(edn: Edn): Either[String, BigDecimal] = edn match {
        case EDecimal(value) => Right(value)
        case _ => readTypeError("BigDecimal", edn)
      }
    }

    implicit object SymbolConverter extends EdnReader[Symbol] with EdnWriter[Symbol] {
      override def write(obj: Symbol): Either[String, Edn] = Right(ESymbol(obj.name))

      override def read(edn: Edn): Either[String, Symbol] = edn match {
        case ESymbol(value) => Right(Symbol(value))
        case _ => readTypeError("Symbol", edn)
      }
    }

    implicit def ListConverter[T](implicit elemWriter: EdnWriter[T], elemReader: EdnReader[T]): EdnReader[List[T]] with EdnWriter[List[T]] =
      new EdnReader[List[T]] with EdnWriter[List[T]] {
        override def write(obj: List[T]): Either[String, Edn] = writeList(obj)

        override def read(edn: Edn): Either[String, List[T]] = edn match {
          case EList(items) => readSeq[T](items, elemReader.read).fold(Left(_), seq => Right(seq.toList))
          case _ => readTypeError("List", edn)
        }
      }

    implicit def VectorConverter[T](implicit elemWriter: EdnWriter[T], elemReader: EdnReader[T]): EdnReader[Vector[T]] with EdnWriter[Vector[T]] =
      new EdnReader[Vector[T]] with EdnWriter[Vector[T]] {
        override def write(obj: Vector[T]): Either[String, Edn] =
          writeList(obj) match {
            case Right(EList(items)) => Right(EVector(items.toVector))
            case x => x
          }

        override def read(edn: Edn): Either[String, Vector[T]] = edn match {
          case EVector(items) => readSeq[T](items, elemReader.read) match {
            case Right(values) => Right(values.toVector)
            case Left(e) => Left(e)
          }
          case _ => readTypeError("Vector", edn)
        }
      }

    implicit def MapConverter[K, V](implicit keyWriter: EdnWriter[K],
                                    keyReader: EdnReader[K],
                                    valueWriter: EdnWriter[V],
                                    valueReader: EdnReader[V]): EdnReader[Map[K, V]] with EdnWriter[Map[K, V]] =
      new EdnReader[Map[K, V]] with EdnWriter[Map[K, V]] {
        override def write(obj: Map[K, V]): Either[String, Edn] =
          obj.foldLeft(Right(EMap(Map.empty)): Either[String, EMap]) {
            case (Right(EMap(map)), (k, v)) =>
              for {
                key <- keyWriter.write(k).right
                value <- valueWriter.write(v).right
              } yield EMap(map + (key -> value))

            case (Left(error), _) => Left(error)
          }

        override def read(edn: Edn): Either[String, Map[K, V]] = edn match {
          case EMap(map) =>
            val items = map.toSeq

            for {
              keys <- readSeq[K](items.map(_._1), keyReader.read).right
              values <- readSeq[V](items.map(_._2), valueReader.read).right
            } yield keys.zip(values).toMap

          case _ => readTypeError("Map", edn)
        }
      }

    implicit def SetConverter[T](implicit elemWriter: EdnWriter[T], elemReader: EdnReader[T]): EdnReader[Set[T]] with EdnWriter[Set[T]] =
      new EdnReader[Set[T]] with EdnWriter[Set[T]] {
        override def write(obj: Set[T]): Either[String, Edn] =
          writeList[T](obj.toSeq) match {
            case Right(elist) => Right(ESet(elist.list.toSet))
            case Left(e) => Left(e)
          }

        override def read(edn: Edn): Either[String, Set[T]] = edn match {
          case ESet(set) => readSeq[T](set.toSeq, elemReader.read) match {
            case Right(values) => Right(values.toSet)
            case Left(e) => Left(e)
          }
          case _ => readTypeError("Set", edn)
        }
      }

    private def readSeq[T](seq: Seq[Edn], f: (Edn) => Either[String, T]): Either[String, Seq[T]] =
      seq.foldLeft(Right(Vector[T]()): Either[String, Vector[T]]) {
        case (Right(result), item) =>
          f(item) match {
            case Right(t) => Right(result :+ t)
            case Left(message) => Left(message)
          }
        case (Left(message), _) => Left(message)
      }

    private def writeList[T](seq: Seq[T])(implicit elemWriter: EdnWriter[T]): Either[String, EList] = {
      val buffer =
        seq.foldLeft(Right(ListBuffer[Edn]()): Either[String, ListBuffer[Edn]]) {
          case (Right(result), item) =>
            elemWriter.write(item) match {
              case Right(edn) => result.append(edn); Right(result)
              case Left(error) => Left(error)
            }
          case (x, _) => x
        }

      buffer match {
        case Right(elems) => Right(EList(elems.toList))
        case Left(message) => Left(message)
      }
    }
  }
}
