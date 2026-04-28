package monads

// монада — контейнер с pure (положить) и flatMap (вытащить и применить функцию)
trait Monad[M[_]]:
  def pure[A](a: A): M[A]
  def flatMap[A, B](ma: M[A])(f: A => M[B]): M[B]
  // map выражается через flatMap, реализовывать отдельно не нужно
  def map[A, B](ma: M[A])(f: A => B): M[B] =
    flatMap(ma)(a => pure(f(a)))

// Semigroup — умеет склеивать два A в один
trait Semigroup[A]:
  def combine(x: A, y: A): A

// Monoid — Semigroup + нейтральный элемент (пустой вектор, пустая строка и т ыд)
trait Monoid[A] extends Semigroup[A]:
  def empty: A

object Monoid:
  // наш лог — Vector[String]: empty = пустой вектор, combine = конкатенация
  given vectorStringMonoid: Monoid[Vector[String]] with
    def empty                                          = Vector.empty
    def combine(x: Vector[String], y: Vector[String]) = x ++ y


// Reader — функция Env => A в обёртке. конфиг не передаём явно везде,
// он сам прокидывается по цепочке и подаётся один раз через .run(config)
final case class Reader[Env, A](run: Env => A):
  // запускаем себя, скармливаем результат в f, f тоже получает тот же env
  def flatMap[B](f: A => Reader[Env, B]): Reader[Env, B] =
    Reader(env => f(run(env)).run(env))
  def map[B](f: A => B): Reader[Env, B] =
    Reader(env => f(run(env)))

object Reader:
  def pure[Env, A](a: A): Reader[Env, A]        = Reader(_ => a)
  def ask[Env]: Reader[Env, Env]                 = Reader(env => env)  // дай всё окружение
  def asks[Env, A](f: Env => A): Reader[Env, A] = Reader(f)           // дай кусок окружения

  // говорим компилятору что Reader — это монада. [A] =>> это «лямбда на уровне типов»
  given readerMonad[Env]: Monad[[A] =>> Reader[Env, A]] with
    def pure[A](a: A): Reader[Env, A]                               = Reader.pure(a)
    def flatMap[A, B](ma: Reader[Env, A])(f: A => Reader[Env, B])
        : Reader[Env, B]                                            = ma.flatMap(f)


// Writer — пара (лог, значение). при каждом flatMap логи склеиваются сами,
// никакого глобального логгера — каждая функция просто возвращает свои строки
final case class Writer[Log, A](run: (Log, A)):
  // применяем f к значению, потом склеиваем log1 ++ log2 через Semigroup
  def flatMap[B](f: A => Writer[Log, B])(using sg: Semigroup[Log]): Writer[Log, B] =
    val (log1, a) = run
    val (log2, b) = f(a).run
    Writer((sg.combine(log1, log2), b))
  def map[B](f: A => B): Writer[Log, B] =
    Writer((run._1, f(run._2)))
  def value: A = run._2
  def log: Log = run._1

object Writer:
  def pure[Log, A](a: A)(using m: Monoid[Log]): Writer[Log, A] = Writer((m.empty, a))
  def tell[Log](entry: Log): Writer[Log, Unit] = Writer((entry, ())) // только запись в лог

  given writerMonad[Log](using Monoid[Log]): Monad[[A] =>> Writer[Log, A]] with
    def pure[A](a: A): Writer[Log, A]                               = Writer.pure(a)
    def flatMap[A, B](ma: Writer[Log, A])(f: A => Writer[Log, B])
        : Writer[Log, B]                                            = ma.flatMap(f)


// State — функция S => (S, A). каждый шаг возвращает новое состояние + результат,
// flatMap сам передаёт новое состояние следующему шагу — никакого var
final case class State[S, A](run: S => (S, A)):
  def flatMap[B](f: A => State[S, B]): State[S, B] =
    State(s =>
      val (s1, a) = run(s) // применяем переход, получаем новое состояние
      f(a).run(s1)         // отдаём новое состояние следующему
    )
  def map[B](f: A => B): State[S, B] =
    State(s =>
      val (s1, a) = run(s)
      (s1, f(a))
    )

object State:
  def pure[S, A](a: A): State[S, A]        = State(s => (s, a))
  def get[S]: State[S, S]                  = State(s => (s, s))      // прочитать состояние
  def set[S](s: S): State[S, Unit]         = State(_ => (s, ()))     // заменить состояние
  def modify[S](f: S => S): State[S, Unit] = State(s => (f(s), ())) // изменить функцией
  def gets[S, A](f: S => A): State[S, A]  = State(s => (s, f(s)))  // прочитать кусок

  given stateMonad[S]: Monad[[A] =>> State[S, A]] with
    def pure[A](a: A): State[S, A]                           = State.pure(a)
    def flatMap[A, B](ma: State[S, A])(f: A => State[S, B])
        : State[S, B]                                        = ma.flatMap(f)


// IO — обёртка над () => A (thunk). ничего не выполняется при создании,
// только когда позовёшь .unsafeRun() — и только в Main
final case class IO[A](unsafeRun: () => A):
  def flatMap[B](f: A => IO[B]): IO[B] = IO(() => f(unsafeRun()).unsafeRun())
  def map[B](f: A => B): IO[B]         = IO(() => f(unsafeRun()))

object IO:
  def pure[A](a: A): IO[A]          = IO(() => a)
  def println(msg: String): IO[Unit] = IO(() => Predef.println(msg))
  val readLine: IO[String]           = IO(() => scala.io.StdIn.readLine())

  given ioMonad: Monad[IO] with
    def pure[A](a: A): IO[A]                            = IO.pure(a)
    def flatMap[A, B](ma: IO[A])(f: A => IO[B]): IO[B] = ma.flatMap(f)
