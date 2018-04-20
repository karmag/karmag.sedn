package karmag.sedn

import karmag.sedn.Edn.Edn

import scala.util.Success

object TestUtil {

  def read(string: String): Edn = {
    EdnIo.read(string)() match {
      case Some(Success(edn)) => edn
      case _ => throw new Exception("boom")
    }
  }
}
