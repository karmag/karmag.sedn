package karmag.sedn

import karmag.sedn.Edn.Edn
import karmag.sedn.EdnConvert.{Base, Converters}
import karmag.sedn.EdnIo.EdnIoMethods
import karmag.sedn.EdnTraverse.EdnTraverseMethods

object Import {

  object All
    extends Base
      with Converters
      with EdnPath.PathOnEdn
      with EdnIoMethods
      with EdnTraverseMethods {
    def path(keys: Edn*): EdnPath.EdnPath = EdnPath.path(keys:_*)
  }
}
