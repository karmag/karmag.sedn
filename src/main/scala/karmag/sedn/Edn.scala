package karmag.sedn

object Edn {
  sealed trait Edn

  // Atoms
  case object ENil extends Edn
  case class EBool(value: Boolean) extends Edn
  case class EString(value: String) extends Edn
  case class EChar(value: Character) extends Edn
  case class EInt(value: BigInt) extends Edn
  case class EDecimal(value: BigDecimal) extends Edn
  case class ERatio(numerator: BigInt, denominator: BigInt) extends Edn
  case class ESymbol(value: String) extends Edn
  case class EKeyword(value: String) extends Edn

  // Collections
  case class EList(list: List[Edn]) extends Edn
  case class EVector(vector: Vector[Edn]) extends Edn
  case class EMap(map: Map[Edn, Edn]) extends Edn
  case class ESet(set: Set[Edn]) extends Edn

  // Meta
  case class ETag(tag: ESymbol, value: Edn) extends Edn
}
