package karmag.sedn

import karmag.sedn.Edn._

import scala.reflect.ClassTag

object EdnSchema {

  trait Schema {
    def check(data: Edn): Edn
    def checkOpt(data: Edn): Option[Edn] =
      check(data) match {
        case ENil => None
        case x => Option(x)
      }
  }
}

object EdnSchemas {
  import karmag.sedn.EdnConvert.Implicits._
  import karmag.sedn.EdnSchema._

  // Schemas that mirrors the edn types.
  private def ednType[T <: Edn](implicit ct: ClassTag[T]): Schema = EdnTypeSchema(ct)
  val eNil     : Schema = ednType[ENil.type]
  val eBool    : Schema = ednType[EBool]
  val eString  : Schema = ednType[EString]
  val eChar    : Schema = ednType[EChar]
  val eInt     : Schema = ednType[EInt]
  val eDecimal : Schema = ednType[EDecimal]
  val eRatio   : Schema = ednType[ERatio]
  val eSymbol  : Schema = ednType[ESymbol]
  val eKeyword : Schema = ednType[EKeyword]
  val eList    : Schema = ednType[EList]
  val eVector  : Schema = ednType[EVector]
  val eMap     : Schema = ednType[EMap]
  val eSet     : Schema = ednType[ESet]
  val eTag     : Schema = ednType[ETag]

  /** Matches any value. */
  def any: Schema = AnySchema

  /** Matches the exact value given. */
  def is(edn: Edn): Schema = IsSchema(edn)

  /** Matches any of the given values. */
  def enum(edn: Edn*): Schema = EnumSchema(edn.toSet)

  /** Match all the given schemas or fail. */
  def and(schema: Schema*): Schema = AndSchema(schema.toList)

  /** Schema that passes if any of the given schemas pass. */
  def or(schema: Schema*): Schema = OrSchema(schema.toList)

  /** Matches tagged value. */
  def tag(symbol: String, value: Schema): Schema = TagSchema(symbol, value)

  /** Matches a sequence with the same number of elements as given
    * schemas. Only matches ordered sequences. */
  def seq(schema: Schema*): Schema = SeqSchema(schema.toList)

  /** Matches a sequence of any length where items match the given schema. */
  def seqOf(schema: Schema): Schema = SeqOfSchema(schema)

  def vector(schema: Schema*): Schema = and(eVector, seq(schema:_*))
  def vectorOf(schema: Schema): Schema = and(eVector, seqOf(schema))
  def list(schema: Schema*): Schema = and(eList, seq(schema:_*))
  def listOf(schema: Schema): Schema = and(eList, seqOf(schema))
  def setOf(schema: Schema): Schema = and(eSet, seqOf(schema))

  /** The resulting schema requires a map in order to validate. Matches
    * keys against their schemas. Additional configuration methods are
    * available on the returned schema. */
  def map(kvs: (MapKey, Schema)*): MapSchema = new MapSchema(kvs.toMap)

  /** Map schema that matches based on the given key and value schemas. */
  def mapOf(keySchema: Schema, valueSchema: Schema): Schema =
    new MapSchema(Map()).allowMore(keySchema, valueSchema)

  sealed trait MapKey
  case class RequiredKey(edn: Edn) extends MapKey
  case class OptionalKey(edn: Edn) extends MapKey

  def req(edn: Edn): MapKey = RequiredKey(edn)
  def opt(edn: Edn): MapKey = OptionalKey(edn)

  /** Schema that uses the given function for validation. Additional
    * configuration methods are available on the returned schema. */
  def pred[T](f: T => Boolean)(implicit ednReader: EdnReader[T], ct: ClassTag[T]): PredicateSchema[T] =
    new PredicateSchema[T](f, ednReader = ednReader, classTag = ct)

  /** Schema that uses the given function for validation. Additional
    * configuration methods are available on the returned schema. */
  def ednPred(pf: PartialFunction[Edn, Boolean]): EdnPredicateSchema = new EdnPredicateSchema(pf)

  /** Checks that numbers, collections and strings are within the given
    * min and max bounds. Bounds are by default inclusive. Additional
    * configuration methods are available on the returned schema.*/
  def range(min: BigInt, max: BigInt): RangeSchema = new RangeSchema(min, max)

  /** Matches schema or nil. */
  def maybe(schema: Schema): Schema = or(eNil, schema)

  /** Matches empty collections or strings. */
  def empty: Schema = EmptySchema

  /** Inverses the given schema. */
  def not(schema: Schema): Schema = NotSchema(schema)

  class MapSchema(kvs: Map[MapKey, Schema],
                  more: Option[(Schema, Schema)] = None) extends Schema {
    private val requiredKvs = kvs.collect { case (RequiredKey(k), s) => k -> s }
    private val optionalKvs = kvs.collect { case (OptionalKey(k), s) => k -> s }
    private val definedKeys = requiredKvs.keySet ++ optionalKvs.keySet

    override def check(data: Edn): Edn = {
      data match {
        case EMap(input) =>

          val reqMap = requiredKvs.foldLeft(Map[Edn, Edn]()) {
            case (map, (key, schema)) =>
              input.get(key) match {
                case Some(value) =>
                  schema.check(value) match {
                    case ENil => map
                    case error => map + (key -> error)
                  }
                case _ => map + (key -> "Missing required key")
              }
          }

          val optMap = optionalKvs.foldLeft(Map[Edn, Edn]()) {
            case (map, (key, schema)) =>
              input.get(key) match {
                case Some(value) =>
                  schema.check(value) match {
                    case ENil => map
                    case error => map + (key -> error)
                  }
                case None => map
              }
          }

          val extraData = input -- definedKeys
          val moreMap = more match {
            case Some((keySchema, valueSchema)) =>
              extraData.foldLeft(Map[Edn, Edn]()) {
                case (map, (key, value)) =>
                  keySchema.check(key) match {
                    case ENil =>
                      valueSchema.check(value) match {
                        case ENil => map
                        case err => map + (key -> err)
                      }
                    case err => map + (key -> EString("Key validation failed: " + EdnIo.compactString(err)))
                  }
              }

            case None =>
              if (extraData.isEmpty)
                Map.empty
              else
                extraData.mapValues(_ => EString("Key not defined"))
          }

          val result = reqMap ++ optMap ++ moreMap
          if (result.isEmpty) ENil
          else                EMap(result)

        case _ => eMap.check(data)
      }
    }

    /** Allow more key values to exist in the map with any shape. */
    def allowMore: MapSchema = allowMore(any, any)

    /** Allow more key values to exist in the map that match the given schemas. */
    def allowMore(key: Schema, value: Schema): MapSchema = new MapSchema(kvs, Option(key -> value))
  }

  class PredicateSchema[T](f: T => Boolean,
                           name: Option[String] = None,
                           message: Option[String] = None,
                           ednReader: EdnReader[T],
                           classTag: ClassTag[T]) extends Schema {
    override def check(data: Edn): Edn =
      ednReader.read(data) match {
        case Right(x) =>
          if (f(x))
            ENil
          else
            message match {
              case Some(fullErrorMessage) => fullErrorMessage
              case None =>
                name match {
                  case Some(str) => s"$str: failed"
                  case None => "Unnamed predicate schema failed"
                }
            }
        case Left(error) =>
          name match {
            case Some(str) => s"$str: failed to convert value: $error"
            case None => s"Failed to convert value: $error"
          }
      }

    /** Sets the name of this predicate. */
    def named(name: String): PredicateSchema[T] =
      new PredicateSchema(f, Option(name), message, ednReader, classTag)

    /** Sets the full error string to use for error reporting. This will
      * override name if that is set. */
    def errorMessage(message: String): PredicateSchema[T] =
      new PredicateSchema(f, name, Option(message), ednReader, classTag)
  }

  class EdnPredicateSchema(pf: PartialFunction[Edn, Boolean],
                           name: Option[String] = None,
                           message: Option[String] = None) extends Schema {
    override def check(data: Edn): Edn =
      if (pf.applyOrElse(data, (_: Edn) => false))
        ENil
      else
        message match {
          case Some(fullErrorMessage) => fullErrorMessage
          case None =>
            name match {
              case Some(str) => s"$str: failed"
              case None => "Unnamed predicate schema failed"
            }
        }

    /** Sets the name of this predicate. */
    def named(name: String): EdnPredicateSchema = new EdnPredicateSchema(pf, Option(name), message)

    /** Sets the full error string to use for error reporting. This will
      * override name if that is set. */
    def errorMessage(message: String): EdnPredicateSchema = new EdnPredicateSchema(pf, name, Option(message))
  }

  class RangeSchema(min: BigInt,
                    max: BigInt,
                    minInclusive: Boolean = true,
                    maxInclusive: Boolean = true) extends Schema {
    override def check(data: Edn): Edn = {
      data match {
        case EString(text)   => check(BigInt(text.length))
        case EChar(char)     => check(BigInt(char.toInt))
        case EInt(value)     => check(value)
        case EDecimal(value) => checkDecimal(value)
        case ERatio(n, d)    => checkDecimal(BigDecimal(n) / BigDecimal(d))
        case EList(list)     => check(list.size)
        case EVector(vector) => check(vector.size)
        case EMap(map)       => check(map.size)
        case ESet(set)       => check(set.size)
        case _ => s"Range schema is not applicable to ${data.getClass.getSimpleName}"
      }
    }

    def minNonInclusive: RangeSchema = new RangeSchema(min, max, false, maxInclusive)
    def maxNonInclusive: RangeSchema = new RangeSchema(min, max, minInclusive, false)

    private def check(bigInt: BigInt): Edn = {
      val minOk =
        if (minInclusive) min <= bigInt
        else              min < bigInt

      val maxOk =
        if (maxInclusive) bigInt <= max
        else              bigInt < max

      if (minOk && maxOk) ENil
      else                s"$bigInt is not in range $min -> $max"
    }

    private def checkDecimal(bigDecimal: BigDecimal): Edn = {
      val minDecimal = BigDecimal(min)
      val maxDecimal = BigDecimal(max)

      val minOk =
        if (minInclusive) minDecimal <= bigDecimal
        else              minDecimal < bigDecimal

      val maxOk =
        if (maxInclusive) bigDecimal <= maxDecimal
        else              bigDecimal < maxDecimal

      if (minOk && maxOk) ENil
      else                s"$bigDecimal is not in range $minDecimal -> $maxDecimal"
    }
  }

  private case object AnySchema extends Schema {
    override def check(data: Edn): Edn = ENil
  }

  private case class AndSchema(schemas: List[Schema]) extends Schema {
    override def check(data: Edn): Edn =
      schemas.map(_.check(data)).find(_ != ENil).getOrElse(ENil)
  }

  private case class OrSchema(schemas: List[Schema]) extends Schema {
    override def check(data: Edn): Edn = check(data, schemas)

    private def check(data: Edn,
                      toTest: List[Schema],
                      error: Edn = ENil): Edn =
      toTest match {
        case Nil => error
        case x :: xs =>
          val result = x.check(data)
          if (result == ENil)
            ENil
          else
            check(data, xs, if (error == ENil) result else error)
      }
  }

  private case class TagSchema(symbol: String, valueSchema: EdnSchema.Schema) extends Schema {
    override def check(data: Edn): Edn =
      data match {
        case ETag(ESymbol(sym), value) =>
          if (symbol != sym)
            Vector(s"Expected tag $symbol but got $sym")
          else {
            valueSchema.check(value) match {
              case ENil => ENil
              case error => EVector(Vector(ENil, error))
            }
          }
        case _ => eTag.check(data)
      }
  }

  private case class SeqSchema(schemas: List[Schema]) extends Schema {
    override def check(data: Edn): Edn =
      data match {
        case EList(items) => check(items, xs => EList(xs.toList))
        case EVector(items) => check(items, xs => EVector(xs.toVector))
        case _ => "Not an ordered sequence"
      }

    private def check(items: Iterable[Edn],
                      wrap: Iterable[Edn] => Edn): Edn = {
      if (items.size != schemas.size)
        s"Expected sequence size ${schemas.size} but got ${items.size}"
      else {
        val result = schemas.zip(items).map { case (s, i) => s.check(i) }
        if (result.exists(_ != ENil))
          wrap(result)
        else
          ENil
      }
    }
  }

  private case class SeqOfSchema(schema: Schema) extends Schema {
    override def check(data: Edn): Edn =
      data match {
        case EList(items) => check(items, xs => EList(xs.toList))
        case EVector(items) => check(items, xs => EVector(xs.toVector))
        case ESet(items) => check(items, xs => ESet(xs.toSet))
        case _ => s"Not a list, vector or set"
      }

    private def check(items: Iterable[Edn],
                      wrap: Iterable[Edn] => Edn): Edn = {
      val result = items.map(schema.check)
      if (result.exists(_ != ENil))
        wrap(result)
      else
        ENil
    }
  }

  private case class IsSchema(edn: Edn) extends Schema {
    override def check(data: Edn): Edn =
      if (edn == data)
        ENil
      else
        s"Expected ${EdnIo.compactString(edn)} but got ${EdnIo.compactString(data)}"
  }

  private case class EnumSchema(values: Set[Edn]) extends Schema {
    override def check(data: Edn): Edn =
      if (values.contains(data))
        ENil
      else
        s"Expected one of ${EdnIo.compactString(ESet(values))} but got ${EdnIo.compactString(data)}"
  }

  private case class EdnTypeSchema[T](ct: ClassTag[T]) extends Schema {
    override def check(data: Edn): Edn =
      if (ct.runtimeClass.isAssignableFrom(data.getClass))
        ENil
      else
        s"Expected type ${ct.runtimeClass.getSimpleName} but got ${data.getClass.getSimpleName}"
  }

  private case object EmptySchema extends Schema {
    override def check(data: Edn): Edn =
      data match {
        case ENil            => ENil
        case EString(str)    => checkInner(str.isEmpty)
        case EList(list)     => checkInner(list.isEmpty)
        case EVector(vector) => checkInner(vector.isEmpty)
        case EMap(map)       => checkInner(map.isEmpty)
        case ESet(set)       => checkInner(set.isEmpty)
        case _ => s"Empty does not work on ${data.getClass.getSimpleName}"
      }

    private def checkInner(empty: Boolean): Edn = {
      if (empty) ENil
      else       EString("Not empty")
    }
  }

  private case class NotSchema(schema: Schema) extends Schema {
    override def check(data: Edn): Edn =
      schema.check(data) match {
        case ENil => s"Not: ${schema.toString}"
        case _ => ENil
      }
  }
}
