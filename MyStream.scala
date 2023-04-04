
trait MyStream[+T] {
  def head: T

  def tail: MyStream[T]

  def headOption: Option[T]

  def isEmpty: Boolean

  def ::[B >: T](element: B): MyStream[B]

  def :::[B >: T](anotherStream: => MyStream[B]): MyStream[B]

  def take(i: Int): MyStream[T]

  def takeWhile(p: T => Boolean): MyStream[T]

  def drop(i: Int): MyStream[T]


  def foreach(f: T => Unit): Unit

  def map[B](f: T => B): MyStream[B]

  def flatMap[B](f: T => MyStream[B]): MyStream[B]

  def filter(f: T => Boolean): MyStream[T]

  def exists(e: T => Boolean): Boolean
}

class EmptyStream extends MyStream[Nothing] {
  override def head: Nothing = throw new RuntimeException("head does not applied on empty stream!")

  override def tail: MyStream[Nothing] = throw new RuntimeException("tail does not applied on empty stream!")

  override def foreach(f: Nothing => Unit): Unit = ()

  override def take(i: Int): MyStream[Nothing] = this

  override def ::[B >: Nothing](element: B): MyStream[B] = new ConsStream[B](element, this)

  override def map[B](f: Nothing => B): MyStream[B] = this

  override def flatMap[B](f: Nothing => MyStream[B]): MyStream[B] = this

  override def :::[B >: Nothing](anotherStream: => MyStream[B]): MyStream[B] = anotherStream

  override def headOption: Option[Nothing] = None

  override def isEmpty: Boolean = true

  override def takeWhile(p: Nothing => Boolean): MyStream[Nothing] = this

  override def drop(i: Int): MyStream[Nothing] = this

  override def filter(f: Nothing => Boolean): MyStream[Nothing] = this

  override def exists(e: Nothing => Boolean): Boolean = false
}

class ConsStream[A](hd: A, tl: => MyStream[A]) extends MyStream[A] {
  override lazy val tail: MyStream[A] = tl
  override val head: A = hd

  override def foreach(f: A => Unit): Unit = {
    f(head)
    tail.foreach(f)
  }

  override def take(i: Int): MyStream[A] = {
    if (i == 0) new EmptyStream
    else if (i == 1) new ConsStream[A](head, new EmptyStream)
    else new ConsStream[A](head, tail.take(i - 1))
  }

  override def ::[B >: A](element: B): MyStream[B] = new ConsStream[B](element, this)

  override def map[B](f: A => B): MyStream[B] = {
    f(head) :: tail.map(f)
  }

  override def flatMap[B](f: A => MyStream[B]): MyStream[B] = {
    f(head) ::: tail.flatMap(f)
  }

  override def :::[B >: A](anotherStream: => MyStream[B]): MyStream[B] = new ConsStream[B](anotherStream.head, anotherStream.tail ::: this)

  override def headOption: Option[A] = Some(head)

  override def isEmpty: Boolean = false

  override def takeWhile(p: A => Boolean): MyStream[A] = {
    if (p(head)) new ConsStream[A](head, tail.takeWhile(p))
    else new EmptyStream()
  }

  override def drop(i: Int): MyStream[A] = {
    if (i <= 0) this
    else if (i == 1) tail
    else tail.drop(i - 1)
  }

  override def filter(f: A => Boolean): MyStream[A] = {
    if (f(head))
      new ConsStream[A](head, tail.filter(f))
    else tail.filter(f)
  }

  override def exists(e: A => Boolean): Boolean = e(head) || tail.exists(e)
}

//Stream Creators
object MyStream {
  def from[A](start: A)(generator: A => A): MyStream[A] = new ConsStream[A](start, from(generator(start))(generator))

  def constant[A](element: A): MyStream[A] = new ConsStream[A](element, constant(element))
}

//Usage Example:
object StreamRunner extends App {
  println("welcome to Stream")

  val infiniteInteger = MyStream.from(1)(r => r + 1)

  infiniteInteger map (_ * 2) take (10) foreach println

}