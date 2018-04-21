package karmag.sedn

import karmag.sedn.Edn._

object EdnMerge {

  /** data patch   -> action
    * map? map?    -> merge, nil values are removed
    * set? set?    -> union
    * set? map?    -> merge, map keys are added, nil values removes
    * vector? map? -> merge, keys must be ints, resize vector if needed
    * _ x          -> x */
  def merge(data: Edn, patch: Edn): Edn = {
    (data, patch) match {
      case (EMap(a), EMap(b)) =>
        val map = b.foldLeft(a) {
          case (result, (k, v)) if v == ENil => result - k
          case (result, (k, v)) => result + (k -> merge(a.getOrElse(k, ENil), v))
        }
        EMap(map)

      case (ESet(a), EMap(b)) =>
        val set = b.foldLeft(a) {
          case (result, (k, ENil)) => result - k
          case (result, (k, _)) => result + k
        }
        ESet(set)

      case (ESet(a), ESet(b)) => ESet(a ++ b)

      case (EVector(a), EMap(b)) =>
        val bInts = b.map {
          case (EInt(n), v) => n.toInt -> v
          case (k, _) => throw new Exception(s"Vector patch data must be integer but was ${EdnIo.compactString(k)}")
        }
        val maxSize = (bInts.keys.max + 1).max(a.size)
        var result = a ++ Vector.fill(0.max(maxSize - a.size))(ENil)

        bInts.foreach {
          case (index, value) =>
            result = result.updated(index, merge(result(index), value))
        }

        EVector(result)

      case _ => patch
    }
  }
}
