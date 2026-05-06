package robot

import monads.IO
import monads.IO.given

final case class MenuLeaf(
    title:  String,
    action: IO[Unit]
) extends MenuOption:
  def show: String = title

final case class MenuTreeNode(
    title:    String,
    children: Seq[MenuOption]
) extends MenuOption with UserInteraction:

  def show: String = title

  private def printMenu: IO[Unit] =
    val lines = children.zipWithIndex.map { (opt, i) => s"  ${i + 1}) ${opt.show}" }
    val body  = (lines :+ "  0) Назад/Выход").mkString("\n")
    IO.println(s"\n=== $title ===\n$body")

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

  def userInteractionLoop: IO[Unit] =
    for
      _      <- printMenu
      answer <- IO.readLine
      _      <- handleUserAnswer(answer)
    yield ()
