package monads

// State[S, A] — обёртка над функцией S => (S, A).
// берёт старое состояние, возвращает новое плюс какое-то значение.
// чисто функциональная замена изменяемому состоянию — никаких var.
final case class State[S, A](run: S => (S, A)):

  // запускает вычисление и применяет f только к значению; состояние не трогаем
  def map[B](f: A => B): State[S, B] =
    State { s =>
      val (s1, a) = run(s)
      (s1, f(a))
    }

  // цепочка: s1 от первого шага попадает во второй как актуальное состояние
  def flatMap[B](f: A => State[S, B]): State[S, B] =
    State { s =>
      val (s1, a) = run(s)
      f(a).run(s1)
    }

object State:

  // читает состояние как значение; ничего не меняет
  def get[S]: State[S, S] = State(s => (s, s))

  // полностью заменяет состояние; старое уходит в /dev/null
  def set[S](s: S): State[S, Unit] = State(_ => (s, ()))

  // прогоняет состояние через функцию f
  def modify[S](f: S => S): State[S, Unit] = State(s => (f(s), ()))

  // нейтральный элемент: состояние не трогает, отдаёт a
  def pure[S, A](a: A): State[S, A] = State(s => (s, a))

  // вытаскивает кусок состояния через f. состояние не меняется.
  def inspect[S, A](f: S => A): State[S, A] =
    State(s => (s, f(s)))

  // [A] =>> State[S, A] — type-lambda: фиксируем S, A остаётся свободным
  given stateMonad[S]: Monad[[A] =>> State[S, A]] with
    def pure[A](a: A): State[S, A] = State.pure(a)
    def flatMap[A, B](ma: State[S, A])(f: A => State[S, B]): State[S, B] =
      ma.flatMap(f)
