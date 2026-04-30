package robot

import monads.{IO, Writer, State, Monad}
import monads.Monoid.given
import monads.IO.given
import robot.Direction.*

// UserInteraction — весь main теперь живёт здесь как дерево MenuTreeNode.
// никаких case "1" / case "2" — меню масштабируется добавлением узлов.
object App extends UserInteraction:

  private val readLine: IO[String]       = IO.readLine
  private def print(s: String): IO[Unit] = IO.println(s)

  // ============================================================================
  // вспомогательные IO
  // ============================================================================

  // печатает Vector[String] построчно — журнал из Writer-цепочки
  private def printLog(log: Vector[String]): IO[Unit] =
    IO(() => log.foreach(println))

  // карта поля: R=робот, X=стена, *=предмет
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

  // ============================================================================
  // flows: каждый — IO[Unit], связывающий Reader + State + Writer
  // ============================================================================

  // шаг в сторону dir. Reader проверяет, можно ли; State обновляет; Writer пишет лог.
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

  // подбор предмета на текущей клетке
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

  // сброс последнего собранного предмета
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

  // зарядка до максимума из конфига
  private def rechargeFlow(cfg: Config, stRef: StRef): IO[Unit] =
    val st            = stRef.get
    val (st1, gained) = StateOps.recharge(cfg.maxEnergy).run(st)
    val Writer(log, _) = WriterOps.logRecharge(gained, cfg.maxEnergy)
    printLog(log).map(_ => stRef.set(st1))

  // показать карту + текстовый статус
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

  // финальная сводка перед выходом
  private def exitSummary(stRef: StRef): IO[Unit] =
    val st = stRef.get
    val items =
      if st.collected.isEmpty then "—"
      else st.collected.map(_.name).mkString(", ")
    print(s"\nИтог: позиция=${st.pos}, энергия=${st.energy}, собрано: $items. До свидания!")

  // ============================================================================
  // мутабельная ссылка на состояние — чтобы все MenuLeaf-замыкания видели один стейт
  // ============================================================================

  // без неё каждый MenuLeaf увидел бы только тот снимок состояния, что был в момент
  // построения дерева
  private final class StRef(private var state: RobotState):
    def get: RobotState              = state
    def set(s: RobotState): Unit     = state = s

  // заголовок-статусбар — пересчитывается каждую итерацию из stRef
  private def statusTitle(stRef: StRef): String =
    val st = stRef.get
    s"Робот: pos=${st.pos} | энергия=${st.energy} | собрано=${st.collected.size}"

  // дерево меню. добавить пункт = вписать MenuLeaf в children.
  // никакого case "N" — нумерацией занимается MenuTreeNode.
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

  // ============================================================================
  // UserInteraction: handleUserAnswer и userInteractionLoop
  // ============================================================================

  // handleUserAnswer делегирует свежепостроенному дереву меню
  def handleUserAnswer(answer: String): IO[Unit] =
    val stRef = StRef(RobotState.initial)
    buildMenu(Config.default, stRef).handleUserAnswer(answer)

  // цикл с обновляемым заголовком: каждый круг пересобираем меню,
  // чтобы статусбар отражал свежее состояние из stRef.
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

  // описание программы. реально что-то произойдёт только при unsafeRun().
  val program: IO[Unit] =
    for
      _ <- print("=== Робот на сетке (вариант 3) ===")
      _ <- userInteractionLoop
    yield ()

  def main(args: Array[String]): Unit =
    program.unsafeRun()
