package karmag.sedn

import karmag.sedn.Edn._

object EdnPath {

  object Implicits extends PathOnEdn

  trait PathOnEdn {
    implicit class RichEdn(edn: Edn) {
      def at(keys: Edn*): PathExt = new PathExt(edn, EdnPath(keys.toList))
      def at(path: EdnPath): PathExt = new PathExt(edn, path)
    }

    implicit class RichEdnPath(ednPath: EdnPath) {
      def withData(edn: Edn): PathExt = new PathExt(edn, ednPath)
    }
  }

  case class EdnPath(keys: List[Edn] = Nil)

  def path(keys: Edn*): EdnPath = EdnPath(keys.toList)

  case class PathError(message: String, path: EdnPath, value: Edn)

  class PathExt(data: Edn, path: EdnPath) {
    def get(): Edn = _root_.karmag.sedn.EdnPath.get(data, path)
    def update(f: Edn => Edn): Edn = _root_.karmag.sedn.EdnPath.update(data, path, f)
    def set(value: Edn): Edn = _root_.karmag.sedn.EdnPath.set(data, path, value)

    def getEither(): Either[PathError, Edn] = _root_.karmag.sedn.EdnPath.getEither(data, path)
    def updateEither(f: Edn => Edn): Either[PathError, Edn] = _root_.karmag.sedn.EdnPath.updateEither(data, path, f)
    def setEither(value: Edn): Either[PathError, Edn] = _root_.karmag.sedn.EdnPath.setEither(data, path, value)
  }

  def get(data: Edn, path: EdnPath): Edn =
    orThrow(getEither(data, path))

  def update(data: Edn, path: EdnPath, f: Edn => Edn): Edn =
    orThrow(updateEither(data, path, f))

  def set(data: Edn, path: EdnPath, value: Edn): Edn =
    orThrow(updateEither(data, path, _ => value))

  def getEither(data: Edn, path: EdnPath): Either[PathError, Edn] = {
    path.keys match {
      case Nil => Right(data)

      case key :: keys =>

        val result: Either[PathError, Edn] =

          data match {

            case ENil => Right(ENil)

            case EList(list) =>
              getInt(key) match {
                case Some(index) =>
                  if (index < list.size) getEither(list(index.toInt), EdnPath(keys))
                  else                   Right(ENil)
                case None =>
                  Left(PathError("List key must be integer", EdnPath(), data))
              }

            case EVector(vector) =>
              getInt(key) match {
                case Some(index) =>
                  if (index < vector.size) getEither(vector(index.toInt), EdnPath(keys))
                  else                     Right(ENil)
                case None =>
                  Left(PathError("Vector key must be integer", EdnPath(), data))
              }

            case EMap(map) =>
              map.get(key) match {
                case Some(value) => getEither(value, EdnPath(keys))
                case None => Right(ENil)
              }

            case ESet(set) =>
              if (set.contains(key)) getEither(key, EdnPath(keys))
              else                   Right(ENil)

            case ETag(_, value) => getEither(value, path)

            case _ =>
              Left(PathError("Can't descend on value", EdnPath(), data))
          }

        result match {
          case Left(error) =>
            Left(error.copy(path = error.path.copy(keys = key :: error.path.keys)))
          case x => x
        }
    }
  }

  def updateEither(data: Edn, path: EdnPath, f: Edn => Edn): Either[PathError, Edn] = {
    path.keys match {

      case Nil => Right(f(data))

      case key :: keys =>

        val result: Either[PathError, Edn] =

          data match {

            case ENil =>
              for {
                replacement <- updateEither(ENil, EdnPath(keys), f).right
              } yield EMap(Map(key -> replacement))

            case EList(list) =>
              getInt(key) match {
                case Some(index) =>
                  val inbounds = index < list.size
                  for {
                    replacement <- if (inbounds) updateEither(list(index), EdnPath(keys), f).right
                                   else          updateEither(ENil, EdnPath(keys), f).right
                  } yield {
                    val fullList =
                      if (inbounds) list
                      else          list ++ List.fill(index - list.size + 1)(ENil)
                    EList(
                      fullList.zipWithIndex.map {
                        case (_   , idx) if idx == index => replacement
                        case (item, _  )                 => item
                      }
                    )
                  }
                case None =>
                  Left(PathError("List key must be integer", EdnPath(), data))
              }

            case EVector(vector) =>
              getInt(key) match {
                case Some(index) =>
                  val inbounds = index < vector.size
                  for {
                    replacement <- if (inbounds) updateEither(vector(index), EdnPath(keys), f).right
                                   else          updateEither(ENil, EdnPath(keys), f).right
                  } yield {
                    val newVector =
                      if (inbounds) vector
                      else          vector ++ Vector.fill(index - vector.size + 1)(ENil)
                    EVector(newVector.updated(index, replacement))
                  }
                case None =>
                  Left(PathError("Vector key must be integer", EdnPath(), data))
              }

            case EMap(map) =>
              for {
                newValue <- updateEither(map.getOrElse(key, ENil), EdnPath(keys), f).right
              } yield EMap(map + (key -> newValue))

            case ESet(set) =>
              for {
                newValue <- updateEither(key, EdnPath(keys), f).right
              } yield {
                if (set.contains(key)) ESet(set - key + newValue)
                else                   ESet(set + newValue)
              }

            case ETag(tag, value) =>
              for {
                newValue <- updateEither(value, path, f).right
              } yield ETag(tag, newValue)

            case _ =>
              Left(PathError("Can't descend on value", EdnPath(), data))
          }

        result match {
          case Left(error) =>
            Left(error.copy(path = error.path.copy(keys = key :: error.path.keys)))
          case x => x
        }
    }
  }

  def setEither(data: Edn, path: EdnPath, value: Edn): Either[PathError, Edn] =
    updateEither(data, path, _ => value)

  private def getInt(edn: Edn): Option[Int] =
    edn match {
      case EInt(int) => Option(int.toInt)
      case ERatio(n, d) if d == 1 => Option(n.toInt)
      case _ => None
    }

  private def orThrow(x: Either[PathError, Edn]): Edn =
    x match {
      case Right(edn) => edn
      case Left(error) => throw new Exception(error.message)
    }
}
