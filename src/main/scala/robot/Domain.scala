package robot

import monads.IO

// позиция на сетке
final case class Pos(x: Int, y: Int):
  override def toString: String = s"($x,$y)"

// куда может пойти робот
enum Direction(val label: String):
  case Up    extends Direction("вверх")
  case Down  extends Direction("вниз")
  case Left  extends Direction("влево")
  case Right extends Direction("вправо")

// предмет на карте
final case class Item(name: String)

// неизменяемая конфигурация — это Env для Reader-монады
final case class Config(
    width:     Int,         // ширина поля
    height:    Int,         // высота поля
    obstacles: Set[Pos],    // клетки-стены
    moveCost:  Int,         // энергия за шаг
    pickCost:  Int,         // энергия за подбор
    maxEnergy: Int          // максимальный заряд
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

// это S для State-монады — всё изменяемое состояние робота
final case class RobotState(
    pos:        Pos,                  // где сейчас
    energy:     Int,                  // сколько энергии
    collected:  Vector[Item],         // что собрано
    itemsOnMap: Map[Pos, Item]        // что лежит на карте
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

// псевдоним для лога Writer-монады
type Log = Vector[String]

// абстракция пункта меню — знает только как себя показать
trait MenuOption:
  def show: String

// абстракция взаимодействия с пользователем через IO.
// handleUserAnswer — обработать ввод. userInteractionLoop — полный цикл.
trait UserInteraction:
  def handleUserAnswer(answer: String): IO[Unit]
  def userInteractionLoop: IO[Unit]

// extension-методы для Item — добавлены снаружи, без наследования
extension (item: Item)
  def summary: String = s"'${item.name}'"
