package robot

import monads.Reader

object ReaderOps:

  def canMove(pos: Pos, dir: Direction): Reader[Config, Boolean] =
    Reader.asks { cfg =>
      val next = nextPos(pos, dir)
      isInsideGrid(next, cfg) && !cfg.obstacles.contains(next)
    }

  def energyCost(action: String): Reader[Config, Int] =
    Reader.asks { cfg =>
      action match
        case "move" => cfg.moveCost
        case "pick" => cfg.pickCost
        case _      => 0
    }

  def isBlocked(cell: Pos): Reader[Config, Boolean] =
    Reader.asks(_.obstacles.contains(cell))

  def insideGrid(cell: Pos): Reader[Config, Boolean] =
    Reader.asks(cfg => isInsideGrid(cell, cfg))

  def nextPos(pos: Pos, dir: Direction): Pos =
    dir match
      case Direction.Up    => pos.copy(y = pos.y - 1)
      case Direction.Down  => pos.copy(y = pos.y + 1)
      case Direction.Left  => pos.copy(x = pos.x - 1)
      case Direction.Right => pos.copy(x = pos.x + 1)

  private def isInsideGrid(cell: Pos, cfg: Config): Boolean =
    cell.x >= 0 && cell.x < cfg.width &&
    cell.y >= 0 && cell.y < cfg.height
