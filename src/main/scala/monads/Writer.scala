package monads

// Writer[Log, A] — пара: накопленный лог Log и значение A.
// лог write-only: внутри цепочки можно только дописывать, читать нельзя.
// в этом проекте Log = Vector[String].
final case class Writer[Log, A](log: Log, value: A):

  // map применяет f только к value; лог не трогается
  def map[B](f: A => B): Writer[Log, B] =
    Writer(log, f(value))

  // using ev: Monoid[Log] — гарантия, что логи можно склеивать.
  // без него flatMap не скомпилится: непонятно, как соединить два лога.
  def flatMap[B](f: A => Writer[Log, B])(using ev: Monoid[Log]): Writer[Log, B] =
    val Writer(log2, b) = f(value)
    Writer(ev.combine(log, log2), b)

// алгебраический интерфейс — то, без чего Writer не работает.
// empty — нейтральный элемент: combine(empty, x) == x.
// combine — ассоциативная операция склейки.
trait Monoid[A]:
  def empty: A
  def combine(x: A, y: A): A

object Monoid:
  // конкретная реализация Monoid для Vector[T]: пустой вектор + конкатенация
  given vectorMonoid[T]: Monoid[Vector[T]] with
    def empty: Vector[T]                               = Vector.empty
    def combine(x: Vector[T], y: Vector[T]): Vector[T] = x ++ y

object Writer:

  // единственный способ записать в лог. value = (), вся суть — в добавлении l.
  def tell[Log](l: Log): Writer[Log, Unit] = Writer(l, ())

  // нейтральный элемент: лог пустой, значение — a
  def pure[Log, A](a: A)(using ev: Monoid[Log]): Writer[Log, A] =
    Writer(ev.empty, a)

  // условный given: instance Monad для Writer[Log, ?] существует
  // ТОЛЬКО если есть Monoid[Log]. компилятор проверит на этапе компиляции.
  given writerMonad[Log](using ev: Monoid[Log]): Monad[[A] =>> Writer[Log, A]] with
    def pure[A](a: A): Writer[Log, A] = Writer.pure(a)
    def flatMap[A, B](ma: Writer[Log, A])(f: A => Writer[Log, B]): Writer[Log, B] =
      ma.flatMap(f)
