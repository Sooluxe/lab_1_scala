package robot

import monads.IO

final case class Pos(x: Int, y: Int):
  override def toString: String = s"($x,$y)"

enum Direction(val label: String):
  case Up    extends Direction("вверх")
  case Down  extends Direction("вниз")
  case Left  extends Direction("влево")
  case Right extends Direction("вправо")

final case class Item(name: String)

final case class Config(
    width:     Int,
    height:    Int,
    obstacles: Set[Pos],
    moveCost:  Int,
    pickCost:  Int,
    maxEnergy: Int
)

object Config:
  val default: Config = Config(
    width     = 5,
    height    = 5,
    obstacles = Set(Pos(2, 2), Pos(1, 3)),
    moveCost  = 5,
    pickCost  = 3,
    maxEnergy = 50
  )

final case class RobotState(
    pos:        Pos,
    energy:     Int,
    collected:  Vector[Item],
    itemsOnMap: Map[Pos, Item]
)

object RobotState:
  val initial: RobotState = RobotState(
    pos        = Pos(0, 0),
    energy     = 30,
    collected  = Vector.empty,
    itemsOnMap = Map(
      Pos(1, 0) -> Item("ключ"),
      Pos(3, 1) -> Item("монета"),
      Pos(0, 4) -> Item("кристалл")
    )
  )

type Log = Vector[String]

trait MenuOption:
  def show: String

trait UserInteraction:
  def handleUserAnswer(answer: String): IO[Unit]
  def userInteractionLoop: IO[Unit]

extension (item: Item)
  def summary: String = s"'${item.name}'"
