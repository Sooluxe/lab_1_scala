package monads

// тайпкласс монады. M[_] — это «коробочка» с одним параметром:
// можно сказать Monad[IO], Monad[List], но не Monad[Int].
trait Monad[M[_]]:

  // нейтральный элемент: pure(a).flatMap(f) == f(a)
  def pure[A](a: A): M[A]

  // склеивает два вычисления — тут и прячется вся специфика конкретной монады
  def flatMap[A, B](ma: M[A])(f: A => M[B]): M[B]

  // map выводится из pure + flatMap, поэтому достаётся бесплатно
  def map[A, B](ma: M[A])(f: A => B): M[B] =
    flatMap(ma)(a => pure(f(a)))

object Monad:
  // расширения для любого M[A], у которого в скоупе лежит given Monad[M].
  // благодаря им можно писать for-comprehension над абстрактной M.
  extension [M[_], A](ma: M[A])(using m: Monad[M])
    def flatMap[B](f: A => M[B]): M[B] = m.flatMap(ma)(f)
    def map[B](f: A => B): M[B]        = m.map(ma)(f)
