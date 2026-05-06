package monads

final case class Writer[Log, A](log: Log, value: A):

  def map[B](f: A => B): Writer[Log, B] =
    Writer(log, f(value))

  def flatMap[B](f: A => Writer[Log, B])(using ev: Monoid[Log]): Writer[Log, B] =
    val Writer(log2, b) = f(value)
    Writer(ev.combine(log, log2), b)

trait Monoid[A]:
  def empty: A
  def combine(x: A, y: A): A

object Monoid:
  given vectorMonoid[T]: Monoid[Vector[T]] with
    def empty: Vector[T]                               = Vector.empty
    def combine(x: Vector[T], y: Vector[T]): Vector[T] = x ++ y

object Writer:

  def tell[Log](l: Log): Writer[Log, Unit] = Writer(l, ())

  def pure[Log, A](a: A)(using ev: Monoid[Log]): Writer[Log, A] =
    Writer(ev.empty, a)

  given writerMonad[Log](using ev: Monoid[Log]): Monad[[A] =>> Writer[Log, A]] with
    def pure[A](a: A): Writer[Log, A] = Writer.pure(a)
    def flatMap[A, B](ma: Writer[Log, A])(f: A => Writer[Log, B]): Writer[Log, B] =
      ma.flatMap(f)
