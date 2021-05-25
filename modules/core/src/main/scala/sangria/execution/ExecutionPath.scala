package sangria.execution

import sangria.marshalling.ResultMarshaller
import sangria.ast
import sangria.schema.ObjectType

case class ExecutionPath private (path: List[Any], cacheKeyPath: List[String]) {
  def add(field: ast.Field, parentType: ObjectType[_, _]) =
    new ExecutionPath(field.outputName :: path, parentType.name :: field.outputName :: cacheKey)

  def withIndex(idx: Int) = new ExecutionPath(idx :: path, cacheKey)

  def isEmpty = path.isEmpty
  def nonEmpty = path.nonEmpty

  /** @return last index in the path, if available
    */
  def lastIndex: Option[Int] = path.headOption.collect { case i: Int => i }

  /** @return the size of the path excluding the indexes
    */
  def size = cacheKeyPath.size / 2

  def marshal(m: ResultMarshaller): m.Node = m.arrayNode(path.map {
    case s: String => m.scalarNode(s, "String", Set.empty)
    case i: Int => m.scalarNode(i, "Int", Set.empty)
  }.toVector.reverse)

  def cacheKey: ExecutionPath.PathCacheKey = cacheKeyPath.toVector

  override def toString = path.foldLeft("") {
    case ("", str: String) => str
    case (acc, str: String) => acc + "." + str
    case (acc, idx: Int) => acc + "[" + idx + "]"

    case ("", other) => other.toString
    case (acc, other) => acc + "." + other.toString
  }
}

object ExecutionPath {
  type PathCacheKey = Vector[String]

  val empty = new ExecutionPath(List.empty, List.empty)
}
