package robot

import monads.Reader

// набор вычислений, которым нужен Config.
// каждая функция возвращает Reader — описание зависимости, а не готовый ответ.
// конфиг не передаётся явно — он подъедет снаружи в момент .run(cfg).
object ReaderOps:

  // можно ли отсюда шагнуть в сторону dir?
  def canMove(pos: Pos, dir: Direction): Reader[Config, Boolean] =
    Reader.asks { cfg =>
      val next = nextPos(pos, dir)
      isInsideGrid(next, cfg) && !cfg.obstacles.contains(next)
    }

  // во сколько энергии обойдётся действие
  def energyCost(action: String): Reader[Config, Int] =
    Reader.asks { cfg =>
      action match
        case "move" => cfg.moveCost
        case "pick" => cfg.pickCost
        case _      => 0
    }

  // стена тут или нет
  def isBlocked(cell: Pos): Reader[Config, Boolean] =
    Reader.asks(_.obstacles.contains(cell))

  // в границах поля или нет
  def insideGrid(cell: Pos): Reader[Config, Boolean] =
    Reader.asks(cfg => isInsideGrid(cell, cfg))

  // куда попадём, если шагнуть в dir. чистая функция — без обёртки в Reader.
  def nextPos(pos: Pos, dir: Direction): Pos =
    dir match
      case Direction.Up    => pos.copy(y = pos.y - 1)
      case Direction.Down  => pos.copy(y = pos.y + 1)
      case Direction.Left  => pos.copy(x = pos.x - 1)
      case Direction.Right => pos.copy(x = pos.x + 1)

  // внутренний хелпер — без Reader, чтобы переиспользовать его внутри canMove
  private def isInsideGrid(cell: Pos, cfg: Config): Boolean =
    cell.x >= 0 && cell.x < cfg.width &&
    cell.y >= 0 && cell.y < cfg.height
