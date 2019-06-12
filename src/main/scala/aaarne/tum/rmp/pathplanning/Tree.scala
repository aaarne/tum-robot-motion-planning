package aaarne.tum.rmp.pathplanning

sealed abstract class Tree[T] {

  def contains(value: T): Boolean = this match {
    case Leaf(v) => v == value
    case Branch(v, children) => v == value || (children exists (c => c contains value))
  }

  def add(parent: T, value: T): Tree[T] = this match {
    case Leaf(v) if v == parent => Branch(v, Leaf(value) :: Nil)
    case Branch(v, children) if v == parent => Branch(v, Leaf(value) :: children)
    case Branch(v, children) =>
      val (does, not) = children partition (c => c contains parent)
      Branch(v, (does map (t => t.add(parent, value))) ::: not)
  }

  def pathTo(value: T): List[T] = {

    def loop(path: List[T], tree: Tree[T]): List[T] = tree match {
      case Leaf(v) if v == value => (value :: path).reverse
      case Branch(v, _) if v == value => (value :: path).reverse
      case Branch(v, children) =>
        children find (c => c contains value) match {
          case None => throw new IllegalArgumentException
          case Some(c) => loop(v :: path, c)
        }
    }

    loop(Nil, this)
  }
}

case class Leaf[T](value: T) extends Tree[T]

case class Branch[T](value: T, children: List[Tree[T]]) extends Tree[T]

