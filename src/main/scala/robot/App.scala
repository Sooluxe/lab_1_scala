package robot

import monads.{IO, Writer, State, Monad}
import monads.Monoid.given
import monads.IO.given
import robot.Direction.*

object App extends UserInteraction:

  private val readLine: IO[String]       = IO.readLine
  private def print(s: String): IO[Unit] = IO.println(s)

  private def printLog(log: Vector[String]): IO[Unit] =
    IO(() => log.foreach(println))

  private def printMap(cfg: Config, st: RobotState): IO[Unit] =
    IO(() =>
      println("\nКарта (R=робот, X=стена, *=предмет):")
      for y <- 0 until cfg.height do
        val row = (0 until cfg.width).map { x =>
          val p = Pos(x, y)
          if      p == st.pos              then "R"
          else if cfg.obstacles(p)         then "X"
          else if st.itemsOnMap.contains(p) then "*"
          else                                  "."
        }.mkString(" ")
        println(s"  $row")
    )

  private def moveFlow(dir: Direction, cfg: Config, stRef: StRef): IO[Unit] =
    val st   = stRef.get
    val to   = ReaderOps.nextPos(st.pos, dir)
    val ok   = ReaderOps.canMove(st.pos, dir).run(cfg)
    val cost = ReaderOps.energyCost("move").run(cfg)

    if !ok then
      val reason = if cfg.obstacles.contains(to) then "препятствие" else "граница поля"
      val Writer(log, _) = WriterOps.logFailedMove(st.pos, dir, reason)
      printLog(log)
    else if st.energy < cost then
      val Writer(log, _) = WriterOps.logFailedMove(st.pos, dir, "недостаточно энергии")
      printLog(log)
    else
      val (st1, _) = StateOps.move(to, cost).run(st)
      val Writer(log, _) =
        for
          _ <- WriterOps.logMove(st.pos, dir, to, cost)
          _ <- WriterOps.logEnergy(st.energy, st1.energy)
        yield ()
      printLog(log).map(_ => stRef.set(st1))

  private def pickFlow(cfg: Config, stRef: StRef): IO[Unit] =
    val st   = stRef.get
    val cost = ReaderOps.energyCost("pick").run(cfg)

    if !st.itemsOnMap.contains(st.pos) then
      val Writer(log, _) = WriterOps.logPickFail(st.pos, "нет предмета")
      printLog(log)
    else if st.energy < cost then
      val Writer(log, _) = WriterOps.logPickFail(st.pos, "недостаточно энергии")
      printLog(log)
    else
      val (st1, picked) = StateOps.pickItem(cost).run(st)
      picked match
        case Some(item) =>
          val Writer(log, _) =
            for
              _ <- WriterOps.logPick(st.pos, item, cost)
              _ <- WriterOps.logEnergy(st.energy, st1.energy)
            yield ()
          printLog(log).map(_ => stRef.set(st1))
        case None =>
          IO.pure(())

  private def dropFlow(stRef: StRef): IO[Unit] =
    val st             = stRef.get
    val (st1, dropped) = StateOps.dropItem.run(st)
    dropped match
      case Some(item) =>
        val Writer(log, _) = WriterOps.logDrop(st.pos, item)
        printLog(log).map(_ => stRef.set(st1))
      case None =>
        val Writer(log, _) = WriterOps.logDropFail
        printLog(log)

  private def rechargeFlow(cfg: Config, stRef: StRef): IO[Unit] =
    val st            = stRef.get
    val (st1, gained) = StateOps.recharge(cfg.maxEnergy).run(st)
    val Writer(log, _) = WriterOps.logRecharge(gained, cfg.maxEnergy)
    printLog(log).map(_ => stRef.set(st1))

  private def showStateFlow(cfg: Config, stRef: StRef): IO[Unit] =
    val st = stRef.get
    for
      _ <- printMap(cfg, st)
      _ <- print(
        s"""Статус:
           |  позиция : ${st.pos}
           |  энергия : ${st.energy}/${cfg.maxEnergy}
           |  собрано : ${if st.collected.isEmpty then "—" else st.collected.map(_.name).mkString(", ")}
           |  на карте: ${if st.itemsOnMap.isEmpty then "—" else st.itemsOnMap.values.map(_.name).mkString(", ")}""".stripMargin
      )
    yield ()

  private def exitSummary(stRef: StRef): IO[Unit] =
    val st = stRef.get
    val items =
      if st.collected.isEmpty then "—"
      else st.collected.map(_.name).mkString(", ")
    print(s"\nИтог: позиция=${st.pos}, энергия=${st.energy}, собрано: $items. До свидания!")

  private final class StRef(private var state: RobotState):
    def get: RobotState              = state
    def set(s: RobotState): Unit     = state = s

  private def statusTitle(stRef: StRef): String =
    val st = stRef.get
    s"Робот: pos=${st.pos} | энергия=${st.energy} | собрано=${st.collected.size}"

  private def buildMenu(cfg: Config, stRef: StRef): MenuTreeNode =
    val movement = MenuTreeNode(
      title = "Движение",
      children = Seq(
        MenuLeaf("Вверх",  moveFlow(Up,    cfg, stRef)),
        MenuLeaf("Вниз",   moveFlow(Down,  cfg, stRef)),
        MenuLeaf("Влево",  moveFlow(Left,  cfg, stRef)),
        MenuLeaf("Вправо", moveFlow(Right, cfg, stRef))
      )
    )
    val items = MenuTreeNode(
      title = "Предметы",
      children = Seq(
        MenuLeaf("Подобрать", pickFlow(cfg, stRef)),
        MenuLeaf("Бросить",   dropFlow(stRef))
      )
    )
    MenuTreeNode(
      title = statusTitle(stRef),
      children = Seq(
        movement,
        items,
        MenuLeaf("Зарядка",        rechargeFlow(cfg, stRef)),
        MenuLeaf("Показать карту", showStateFlow(cfg, stRef))
      )
    )

  def handleUserAnswer(answer: String): IO[Unit] =
    val stRef = StRef(RobotState.initial)
    buildMenu(Config.default, stRef).handleUserAnswer(answer)

  def userInteractionLoop: IO[Unit] =
    val stRef = StRef(RobotState.initial)
    val cfg   = Config.default

    def loop: IO[Unit] =
      val menu = buildMenu(cfg, stRef)
      for
        _      <- IO.println(s"\n=== ${statusTitle(stRef)} ===")
        _      <- IO(() =>
                    menu.children.zipWithIndex.foreach { (opt, i) =>
                      println(s"  ${i + 1}) ${opt.show}")
                    }
                    println("  0) Выход")
                  )
        answer <- readLine
        _      <- answer.trim match
                    case "0" | "" =>
                      exitSummary(stRef)
                    case s =>
                      s.toIntOption match
                        case Some(n) if n >= 1 && n <= menu.children.size =>
                          menu.children(n - 1) match
                            case leaf: MenuLeaf     => leaf.action.flatMap(_ => loop)
                            case node: MenuTreeNode => node.userInteractionLoop.flatMap(_ => loop)
                            case other              => IO.println(s"Неизвестно: ${other.show}").flatMap(_ => loop)
                        case _ =>
                          IO.println(s"Неизвестная команда: '$s'").flatMap(_ => loop)
      yield ()
    loop

  val program: IO[Unit] =
    for
      _ <- print("=== Робот на сетке (вариант 3) ===")
      _ <- userInteractionLoop
    yield ()

  def main(args: Array[String]): Unit =
    program.unsafeRun()
