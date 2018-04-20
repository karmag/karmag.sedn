package karmag.sedn

import karmag.sedn.Edn._

import scala.collection.mutable.ListBuffer

/** Traversal of EDN data.
  *
  * Functions in this object performs depth-first traversal descending
  * recursively to the items of collections. `pre` arguments are applied
  * before descending and `post` arguments are applied after. */
object EdnTraverse {

  object Implicits extends EdnTraverseMethods

  trait EdnTraverseMethods {
    implicit class RichEdnTraverse(edn: Edn) {
      def walk[T](state: T,
                  pre: PartialFunction[(Edn, T), (Edn, T)] = PartialFunction.empty,
                  post: PartialFunction[(Edn, T), (Edn, T)] = PartialFunction.empty): (Edn, T) =
        EdnTraverse.walk(edn, state, pre, post)

      def inspect[T](state: T,
                     pre: PartialFunction[(Edn, T), T] = PartialFunction.empty,
                     post: PartialFunction[(Edn, T), T] = PartialFunction.empty): T =
        EdnTraverse.inspect(edn, state, pre, post)

      def transform(pre: PartialFunction[Edn, Edn] = PartialFunction.empty,
                    post: PartialFunction[Edn, Edn] = PartialFunction.empty): Edn =
        EdnTraverse.transform(edn, pre, post)
    }
  }

  /** Traverse the data potentially changing it and collect information.
    *
    * This function performs multiple services. If only transformation or
    * inspection is wanted the corresponding functions should be used instead. */
  def walk[T](data: Edn,
              state: T,
              pre: PartialFunction[(Edn, T), (Edn, T)] = PartialFunction.empty,
              post: PartialFunction[(Edn, T), (Edn, T)] = PartialFunction.empty): (Edn, T) = {
    val (preData, preState) = pre.applyOrElse((data, state), (_: Any) => (data, state))

    val walked =
      preData match {
        case EList(list) =>
          val (listData, listState) =
            list.foldLeft((ListBuffer[Edn](), preState)) {
              case ((acc, innerState), item) =>
                val (newItem, newState) = walk(item, innerState, pre, post)
                acc.append(newItem)
                acc -> newState
            }
          EList(listData.toList) -> listState

        case EVector(vector) =>
          val (vecData, vecState) =
            vector.foldLeft((Vector[Edn](), preState)) {
              case ((acc, innerState), item) =>
                val (newItem, newState) = walk(item, innerState, pre, post)
                (acc :+ newItem) -> newState
            }
          EVector(vecData) -> vecState

        case EMap(map) =>
          val (mapData, mapState) =
            map.foldLeft((Map[Edn, Edn](), preState)) {
              case ((acc, innerState), (k, v)) =>
                val (keyItem, keyState) = walk(k, innerState, pre, post)
                val (valueItem, valueState) = walk(v, keyState, pre, post)
                (acc + (keyItem -> valueItem)) -> valueState
            }
          EMap(mapData) -> mapState

        case ESet(set) =>
          val (setData, setState) =
            set.foldLeft((Set[Edn](), preState)) {
              case ((acc, innerState), item) =>
                val (newItem, newState) = walk(item, innerState, pre, post)
                (acc + newItem) -> newState
            }
          ESet(setData) -> setState

        case ETag(tag, value) =>
          val (newValue, newState) = walk(value, preState, pre, post)
          ETag(tag, newValue) -> newState

        case _ => preData -> preState
      }

    post.applyOrElse(walked, (_: Any) => walked)
  }

  /** Traverse the data without altering it. State can be used to collect
    * information. */
  def inspect[T](data: Edn,
                 state: T,
                 pre: PartialFunction[(Edn, T), T] = PartialFunction.empty,
                 post: PartialFunction[(Edn, T), T] = PartialFunction.empty): T = {
    val newState = pre.applyOrElse((data, state), (_: (Edn, T)) => state)

    val preProcessedState =
      data match {
        case EList(list)     => inspectIterable(list, newState, pre, post)
        case EVector(vector) => inspectIterable(vector, newState, pre, post)
        case EMap(map)       => inspectIterable(map.keys ++ map.values, newState, pre, post)
        case ESet(set)       => inspectIterable(set, newState, pre, post)
        case ETag(_, value)  => inspect(value, newState, pre, post)
        case _               => newState
      }

    post.applyOrElse((data, preProcessedState), (_: (Edn, T)) => preProcessedState)
  }

  private def inspectIterable[T](iterable: Iterable[Edn],
                                 state: T,
                                 pre: PartialFunction[(Edn, T), T],
                                 post: PartialFunction[(Edn, T), T]): T =
    iterable.foldLeft(state) {
      case (innerState, item) => inspect(item, innerState, pre, post)
    }

  def transform(data: Edn,
                pre: PartialFunction[Edn, Edn] = PartialFunction.empty,
                post: PartialFunction[Edn, Edn] = PartialFunction.empty): Edn = {
    val preFormed = pre.applyOrElse(data, (_: Any) => data)

    val walked =
      preFormed match {
        case EList(list)         => EList(list.map(transform(_, pre, post)))
        case EVector(vector)     => EVector(vector.map(transform(_, pre, post)))
        case EMap(map)           => EMap(map.map {
          case (k, v) => transform(k, pre, post) -> transform(v, pre, post)
        })
        case ESet(set)           => ESet(set.map(transform(_, pre, post)))
        case ETag(symbol, value) => ETag(symbol, transform(value, pre, post))
        case other               => other
      }

    post.applyOrElse(walked, (_: Any) => walked)
  }
}
