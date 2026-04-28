package domain

import monads.*
import monads.Monoid.given

// позиция на сетке
final case class Pos(x: Int, y: Int)

// куда может пойти робот
enum Direction:
  case Up, Down, Left, Right

// предмет, который лежит на карте
final case class Item(name: String)

// вся конфигурация мира — именно это будет «окружением» в Reader
final case class Config(
  width: Int,           // ширина поля
  height: Int,          // высота поля
  obstacles: Set[Pos],  // клетки-стены
  moveCost: Int,        // энергия за шаг
  pickCost: Int,        // энергия за подбор
  maxEnergy: Int        // максимальный заряд
)

// всё изменяемое состояние робота — передаётся через State
final case class RobotState(
  pos: Pos,                  // где сейчас
  energy: Int,               // сколько энергии
  collected: Vector[Item],   // что уже собрал
  itemsOnMap: Map[Pos, Item] // что ещё лежит на карте
)

// лог — просто вектор строк, Writer будет их склеивать
type Log = Vector[String]



// блок 1 Reader — читаем конфиг, не передавая его явно в каждую функцию


object ReaderOps:

  // можно ли сделать шаг в сторону dir отсюда?
  def canMove(pos: Pos, dir: Direction): Reader[Config, Boolean] =
    Reader.asks { cfg =>
      val next = nextPos(pos, dir)
      insideGrid(next, cfg) && !cfg.obstacles.contains(next)
    }

  // сколько энергии стоит действие
  def energyCost(action: String): Reader[Config, Int] =
    Reader.asks { cfg =>
      action match
        case "move" => cfg.moveCost
        case "pick" => cfg.pickCost
        case _      => 0
    }

  // стена или нет
  def isBlocked(cell: Pos): Reader[Config, Boolean] =
    Reader.asks(_.obstacles.contains(cell))

  // в границах поля или нет
  def insideGrid(cell: Pos): Reader[Config, Boolean] =
    Reader.asks(cfg => insideGrid(cell, cfg))

  // куда попадём если шагнуть в dir
  def nextPos(pos: Pos, dir: Direction): Pos =
    dir match
      case Direction.Up    => pos.copy(y = pos.y - 1)
      case Direction.Down  => pos.copy(y = pos.y + 1)
      case Direction.Left  => pos.copy(x = pos.x - 1)
      case Direction.Right => pos.copy(x = pos.x + 1)

  // нужна внутри Reader.asks — без Reader-обёртки
  private def insideGrid(cell: Pos, cfg: Config): Boolean =
    cell.x >= 0 && cell.x < cfg.width &&
    cell.y >= 0 && cell.y < cfg.height


// блок 2  Writer — каждая функция пишет свои строки в лог, flatMap сам их склеивает — никакого глобального логгера


object WriterOps:

  // лог шага: откуда, куда, сколько стоило
  def logMove(from: Pos, dir: Direction, to: Pos, cost: Int): Writer[Log, Unit] =
    Writer.tell(Vector(s"[MOVE] $from -> $dir -> $to  (энергия: -$cost)"))

  // лог неудачного шага с причиной
  def logFailedMove(from: Pos, dir: Direction, reason: String): Writer[Log, Unit] =
    Writer.tell(Vector(s"[FAIL] попытка $dir из $from отклонена: $reason"))

  // лог подбора предмета
  def logPick(pos: Pos, item: Item, cost: Int): Writer[Log, Unit] =
    Writer.tell(Vector(s"[PICK] подобран '${item.name}' на $pos  (энергия: -$cost)"))

  // лог изменения энергии
  def logEnergy(before: Int, after: Int): Writer[Log, Unit] =
    Writer.tell(Vector(s"[NRG]  энергия: $before -> $after"))

  // лог зарядки
  def logRecharge(amount: Int, after: Int): Writer[Log, Unit] =
    Writer.tell(Vector(s"[CHG]  зарядка +$amount, теперь $after"))

  // лог сброса предмета
  def logDrop(pos: Pos, item: Item): Writer[Log, Unit] =
    Writer.tell(Vector(s"[DROP] брошен '${item.name}' на $pos"))

// блок 3. State — переходы состояния каждая функция: старый RobotState => (новый RobotState, результат) цепочка flatMap сама прокидывает состояние от шага к шагу

object StateOps:
  import ReaderOps.*
  import WriterOps.*

  // Result = Writer с логом и опциональной ошибкой (None = всё ок)
  type Result = Writer[Log, Option[String]]

  // движение в сторону dir с учётом конфига
  def move(dir: Direction, cfg: Config): State[RobotState, Result] =
    State { st =>
      val ok   = canMove(st.pos, dir).run(cfg) // спрашиваем Reader
      val cost = energyCost("move").run(cfg)
      if !ok then
        // стена или граница — состояние не трогаем
        val reason = if cfg.obstacles.contains(nextPos(st.pos, dir))
                     then "препятствие"
                     else "граница поля"
        val w: Result = logFailedMove(st.pos, dir, reason).map(_ => Some(reason))
        (st, w)
      else if st.energy < cost then
        val w: Result = logFailedMove(st.pos, dir, "недостаточно энергии")
                          .map(_ => Some("недостаточно энергии"))
        (st, w)
      else
        val to    = nextPos(st.pos, dir)
        val newE  = st.energy - cost
        val newSt = st.copy(pos = to, energy = newE)
        // for-comprehension на Writer: логи склеятся сами через flatMap
        val w: Result = for
          _ <- logMove(st.pos, dir, to, cost)
          _ <- logEnergy(st.energy, newE)
        yield None
        (newSt, w)
    }

  // подбор предмета на текущей клетке
  def pickItem(cfg: Config): State[RobotState, Result] =
    State { st =>
      val cost = energyCost("pick").run(cfg)
      st.itemsOnMap.get(st.pos) match
        case None =>
          val w: Result = Writer.tell[Log](Vector(s"[INFO] нет предмета на ${st.pos}"))
                                .map(_ => Some("нет предмета"))
          (st, w)
        case Some(item) if st.energy < cost =>
          val w: Result = Writer.tell[Log](Vector(s"[FAIL] подбор: недостаточно энергии"))
                                .map(_ => Some("недостаточно энергии"))
          (st, w)
        case Some(item) =>
          val newE  = st.energy - cost
          val newSt = st.copy(
            energy     = newE,
            collected  = st.collected :+ item,
            itemsOnMap = st.itemsOnMap - st.pos
          )
          val w: Result = for
            _ <- logPick(st.pos, item, cost)
            _ <- logEnergy(st.energy, newE)
          yield None
          (newSt, w)
    }

  // зарядка до максимума
  def recharge(cfg: Config): State[RobotState, Result] =
    State { st =>
      val gained = cfg.maxEnergy - st.energy
      val newSt  = st.copy(energy = cfg.maxEnergy)
      val w: Result = logRecharge(gained, cfg.maxEnergy).map(_ => None)
      (newSt, w)
    }

  // выбросить последний собранный предмет на текущую клетку
  def dropItem: State[RobotState, Result] =
    State { st =>
      st.collected.lastOption match
        case None =>
          val w: Result = Writer.tell[Log](Vector("[INFO] нечего бросать"))
                                .map(_ => Some("инвентарь пуст"))
          (st, w)
        case Some(item) =>
          val newSt = st.copy(
            collected  = st.collected.dropRight(1),
            itemsOnMap = st.itemsOnMap + (st.pos -> item)
          )
          val w: Result = logDrop(st.pos, item).map(_ => None)
          (newSt, w)
    }
