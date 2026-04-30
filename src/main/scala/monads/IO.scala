package monads

// IO[A] — это описание вычисления с побочными эффектами, которое вернёт A.
// сама лямбда unsafeRun не запускается, пока её явно не позовут — вся программа
// сначала собирается как одно большое описание, а исполняется только в main.
final case class IO[A](unsafeRun: () => A):

  def map[B](f: A => B): IO[B] =
    IO(() => f(unsafeRun()))

  // делает новый IO: сначала запустит this, передаст результат в f, затем
  // запустит и его IO. for-comprehension просто строит вложенное описание —
  // ничего не выполняется до unsafeRun() в main.
  def flatMap[B](f: A => IO[B]): IO[B] =
    IO(() => f(unsafeRun()).unsafeRun())

object IO:

  // готовое значение в IO, без эффектов
  def pure[A](a: A): IO[A] = IO(() => a)

  // a: => A — это call-by-name: a не вычисляется в момент передачи.
  // delay откладывает вычисление, в отличие от pure, которой нужно готовое A.
  def delay[A](a: => A): IO[A] = IO(() => a)

  // обёртка над println
  def println(msg: String): IO[Unit] = IO(() => Predef.println(msg))

  // обёртка над чтением строки. Option(...) защищает от null при EOF.
  val readLine: IO[String] =
    IO(() => Option(scala.io.StdIn.readLine()).getOrElse(""))

  // given — свидетельство, что IO это монада. у IO один параметр типа,
  // поэтому никакой type-lambda тут не нужно.
  given ioMonad: Monad[IO] with
    def pure[A](a: A): IO[A] = IO.pure(a)
    def flatMap[A, B](ma: IO[A])(f: A => IO[B]): IO[B] = ma.flatMap(f)
