package robot

import monads.IO
import monads.IO.given

// листовой пункт меню — название и действие, которое запустится при выборе.
// action — это IO[Unit]: описание того, что произойдёт, без немедленного запуска.
final case class MenuLeaf(
    title:  String,
    action: IO[Unit]
) extends MenuOption:
  def show: String = title

// узел дерева меню — заголовок и список дочерних пунктов.
// сам он одновременно и пункт (MenuOption), и обработчик ввода (UserInteraction).
// children нумеруются автоматически — никаких хардкоженных "1", "2", "3".
final case class MenuTreeNode(
    title:    String,
    children: Seq[MenuOption]
) extends MenuOption with UserInteraction:

  def show: String = title

  // печатает пронумерованное меню. zipWithIndex даёт пары (элемент, индекс),
  // +1 — чтобы нумерация начиналась с единицы.
  private def printMenu: IO[Unit] =
    val lines = children.zipWithIndex.map { (opt, i) => s"  ${i + 1}) ${opt.show}" }
    val body  = (lines :+ "  0) Назад/Выход").mkString("\n")
    IO.println(s"\n=== $title ===\n$body")

  // обрабатывает строку ввода:
  //   "0" или пусто  — выход (IO.pure(())),
  //   число в диапазоне — выбрать пункт: листу позвать action, узлу — войти в подменю,
  //   что-то иное — сообщить об ошибке и вернуться в тот же цикл.
  def handleUserAnswer(answer: String): IO[Unit] =
    answer.trim match
      case "0" | "" =>
        IO.pure(())
      case s =>
        s.toIntOption match
          case Some(n) if n >= 1 && n <= children.size =>
            children(n - 1) match
              case leaf: MenuLeaf =>
                leaf.action.flatMap(_ => userInteractionLoop)
              case node: MenuTreeNode =>
                node.userInteractionLoop.flatMap(_ => userInteractionLoop)
              case other =>
                IO.println(s"Неизвестный пункт: ${other.show}")
                  .flatMap(_ => userInteractionLoop)
          case _ =>
            IO.println(s"Неизвестная команда: '$s'")
              .flatMap(_ => userInteractionLoop)

  // полный цикл: показать меню → прочитать ввод → обработать.
  // рекурсия закрывается через handleUserAnswer.
  def userInteractionLoop: IO[Unit] =
    for
      _      <- printMenu
      answer <- IO.readLine
      _      <- handleUserAnswer(answer)
    yield ()
