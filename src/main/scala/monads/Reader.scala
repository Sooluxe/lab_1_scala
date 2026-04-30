package monads

// Reader[Env, A] — обёртка над функцией Env => A.
// моделирует вычисление, которому для результата A нужна среда Env (например, конфиг).
// среда read-only: читать можно, менять — нет.
final case class Reader[Env, A](run: Env => A):

  // применяет f к результату; среда не трогается
  def map[B](f: A => B): Reader[Env, B] =
    Reader(env => f(run(env)))

  // одна и та же env идёт в оба вычисления. в этом и смысл Reader —
  // среда сама протаскивается через всю цепочку flatMap.
  def flatMap[B](f: A => Reader[Env, B]): Reader[Env, B] =
    Reader(env => f(run(env)).run(env))

object Reader:

  // отдаёт всю среду как значение
  def ask[Env]: Reader[Env, Env] = Reader(identity)

  // достаёт конкретное поле из окружения
  def asks[Env, A](f: Env => A): Reader[Env, A] = Reader(f)

  // нейтральный элемент: окружение игнорится
  def pure[Env, A](a: A): Reader[Env, A] = Reader(_ => a)

  // [A] =>> Reader[Env, A] — type-lambda: фиксирует Env, оставляет A свободным.
  // без неё нельзя — Monad ждёт M[_] с одним параметром, а у Reader их два.
  given readerMonad[Env]: Monad[[A] =>> Reader[Env, A]] with
    def pure[A](a: A): Reader[Env, A] = Reader.pure(a)
    def flatMap[A, B](ma: Reader[Env, A])(f: A => Reader[Env, B]): Reader[Env, B] =
      ma.flatMap(f)
