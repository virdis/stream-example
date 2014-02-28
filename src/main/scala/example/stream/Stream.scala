package example.stream

sealed abstract class Stream[+A] {
  import Stream._

  def uncons: Option[Cons[A]]
  def isEmpty: Boolean = uncons.isEmpty

  def toList: List[A] = {
    val buffer = new collection.mutable.ListBuffer[A]
    def go(s: Stream[A]): List[A] = s uncons match {
      case Some(x) => buffer += x.head; go(x.tail)
      case _ => buffer.toList
    }
    go(this)
  }

  def take(n:Int): Stream[A] = 
    if (n > 0) {
      uncons match {
        case Some(x) => cons[A](x.head, x.tail.take(n-1))
        case _ => Stream()
      }
    } else Stream()
}

case object Empty extends Stream[Nothing] { val uncons = None } 

sealed abstract class Cons[+A] extends Stream[A] {
  def head: A
  def tail: Stream[A]
  val uncons = Some(this)
}


object Stream {
  
  def empty[A]: Stream[A] = Empty

  def cons[A](hd: A, t: => Stream[A]): Stream[A] = new Cons[A] {
    lazy val head = hd
    lazy val tail = t
  }

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) Empty else cons(as.head, apply(as.tail: _*))
}
