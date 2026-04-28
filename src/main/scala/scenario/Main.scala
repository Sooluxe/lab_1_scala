package scenario

import monads.*
import monads.Monoid.given
import domain.*
import domain.Direction.*
import domain.ReaderOps.*
import domain.WriterOps.*
import domain.StateOps.*
import menu.*

// блок 4. IO + меню как данные. диспатчер ничего не знает о конкретных командах,
// он работает только с MenuItem — добавляем новый пункт = добавляем в список

object Main extends App:

  // конфиг мира
  val config = Config(
    width     = 5,
    height    = 5,
    obstacles = Set(Pos(2, 2), Pos(1, 3)),
    moveCost  = 5,
    pickCost  = 3,
    maxEnergy = 50
  )

  // начальное состояние
  val initialState = RobotState(
    pos        = Pos(0, 0),
    energy     = 30,
    collected  = Vector.empty,
    itemsOnMap = Map(
      Pos(1, 0) -> Item("ключ"),
      Pos(3, 1) -> Item("монета"),
      Pos(0, 4) -> Item("кристалл")
    )
  )

  // ============================================================================
  // меню как ДАННЫЕ — никакого хардкода в диспатчере
  // добавить новую команду = вписать новую строчку в список
  // ============================================================================

  // подменю движения
  val movementMenu: Menu = Menu(
    title = "Движение",
    items = List(
      MenuItem("Вверх",   MenuAction.Run(move(Up,    config))),
      MenuItem("Вниз",    MenuAction.Run(move(Down,  config))),
      MenuItem("Влево",   MenuAction.Run(move(Left,  config))),
      MenuItem("Вправо",  MenuAction.Run(move(Right, config))),
      MenuItem("Назад",   MenuAction.Back)
    )
  )

  // подменю предметов
  val itemsMenu: Menu = Menu(
    title = "Предметы",
    items = List(
      MenuItem("Подобрать", MenuAction.Run(pickItem(config))),
      MenuItem("Бросить",   MenuAction.Run(dropItem)),
      MenuItem("Назад",     MenuAction.Back)
    )
  )

  // главное меню — корень дерева
  val mainMenu: Menu = Menu(
    title = "Главное меню",
    items = List(
      MenuItem("Движение",     MenuAction.Goto(movementMenu)),
      MenuItem("Предметы",     MenuAction.Goto(itemsMenu)),
      MenuItem("Зарядка",      MenuAction.Run(recharge(config))),
      MenuItem("Выход",        MenuAction.Quit)
    )
  )

  // ============================================================================
  // запуск перехода + печать его лога
  // ============================================================================

  // выполняет один шаг State, разворачивает Writer, печатает лог
  def runStep(label: String, t: State[RobotState, Result], st: RobotState): RobotState =
    val (newState, writerResult) = t.run(st)
    val (log, maybeErr)         = writerResult.run
    println(s"\n── $label ──")
    log.foreach(println)
    maybeErr.foreach(err => println(s"  ⚠ $err"))
    newState

  // ============================================================================
  // рендер: меню → строки на экране. номера проставляются автоматически
  // ============================================================================

  def printMap(st: RobotState): IO[Unit] =
    IO.println("\nКарта (R=робот, X=стена, *=предмет):") *>
    IO(() =>
      for y <- 0 until config.height do
        val row = (0 until config.width).map { x =>
          val p = Pos(x, y)
          if p == st.pos                    then "R"
          else if config.obstacles(p)       then "X"
          else if st.itemsOnMap.contains(p) then "*"
          else "."
        }.mkString(" ")
        println(s"  $row")
    )

  def printStatus(st: RobotState): IO[Unit] =
    IO.println(
      s"""
         |Статус:
         |  позиция : ${st.pos}
         |  энергия : ${st.energy}/${config.maxEnergy}
         |  собрано : ${if st.collected.isEmpty then "—" else st.collected.map(_.name).mkString(", ")}
         |  на карте: ${st.itemsOnMap.values.map(_.name).mkString(", ")}""".stripMargin
    )

  // печатаем заголовок меню и пронумерованный список пунктов
  def renderMenu(m: Menu): IO[Unit] =
    IO(() =>
      println(s"\n— ${m.title} —")
      m.items.zipWithIndex.foreach { case (item, idx) =>
        println(s"  ${idx + 1}. ${item.label}")
      }
    )

  // ============================================================================
  // диспатчер: ОДИН на всё приложение. он не знает о конкретных командах,
  // только о MenuAction. это и есть OCP — открыт для расширения, закрыт для правки
  // ============================================================================

  // stack — стек меню: голова это текущее, остальное — родители для Back
  def runMenu(stack: List[Menu], st: RobotState): IO[Unit] =
    stack match
      case Nil => IO.pure(()) // некуда дальше — выходим
      case current :: parents =>
        for
          _   <- printMap(st)
          _   <- printStatus(st)
          _   <- renderMenu(current)
          _   <- IO.println("\nВыбор (число):")
          raw <- IO.readLine
          _   <- dispatch(stack, raw, st)
        yield ()

  // парсим число, ищем пункт, выполняем его MenuAction
  def dispatch(stack: List[Menu], raw: String, st: RobotState): IO[Unit] =
    val current :: parents = stack: @unchecked
    parseChoice(raw, current.items.size) match
      case None =>
        IO.println(s"Неверный ввод: '${Option(raw).getOrElse("")}'") *>
        IO(() => runMenu(stack, st).unsafeRun())

      case Some(idx) =>
        val item = current.items(idx)
        item.action match
          case MenuAction.Run(t) =>
            val newSt = runStep(item.label, t, st)
            IO(() => runMenu(stack, newSt).unsafeRun())

          case MenuAction.Goto(sub) =>
            IO(() => runMenu(sub :: stack, st).unsafeRun())

          case MenuAction.Back =>
            parents match
              case Nil => IO(() => runMenu(stack, st).unsafeRun()) // на корне Back = ничего
              case _   => IO(() => runMenu(parents, st).unsafeRun())

          case MenuAction.Quit =>
            IO.println(s"\nСобрано предметов: ${st.collected.size}") *>
            IO.println("До свидания!")

  // безопасный парсинг номера пункта
  def parseChoice(raw: String, size: Int): Option[Int] =
    Option(raw).flatMap(_.trim.toIntOption).map(_ - 1).filter(i => i >= 0 && i < size)

  // ============================================================================
  // демо: автоматический прогон без меню
  // ============================================================================

  def demo(): Unit =
    println("═══════════════════════════════════════")
    println("  Демо: автоматический прогон")
    println("═══════════════════════════════════════")

    var st = initialState
    val steps = List(
      ("Шаг вправо",      move(Right, config)),
      ("Подбор предмета", pickItem(config)),
      ("Шаг вниз",        move(Down,  config)),
      ("Шаг вправо",      move(Right, config)),
      ("В стену (2,2)",   move(Down,  config)),
      ("Зарядка",         recharge(config)),
      ("Сброс предмета",  dropItem)
    )
    for (label, t) <- steps do st = runStep(label, t, st)

    println("\n═══════════════════════════════════════")
    println(s"  Итог: позиция=${st.pos}, энергия=${st.energy}, собрано=${st.collected.map(_.name)}")
    println("═══════════════════════════════════════")

  // ============================================================================
  // точка входа
  // ============================================================================

  demo()

  println("\n═══════════════════════════════════════")
  println("  Интерактивный режим (меню-дерево)")
  println("═══════════════════════════════════════")

  // unsafeRun — единственное место где IO реально исполняется
  runMenu(List(mainMenu), initialState).unsafeRun()


// extension: *> — выполнить левое IO, отбросить результат, выполнить правое
extension [A](io: IO[A])
  def *>[B](next: IO[B]): IO[B] = io.flatMap(_ => next)
