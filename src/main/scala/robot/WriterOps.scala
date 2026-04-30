package robot

import monads.{Writer, Monoid}
// этот импорт втягивает в скоуп given vectorMonoid.
// без него flatMap у Writer не найдёт Monoid[Log] и проект не скомпилится.
import monads.Monoid.given

// каждая функция дописывает свою строку в лог. flatMap сам склеит логи через Monoid.
object WriterOps:

  // мелкий хелпер: оборачивает строку в Vector и зовёт Writer.tell
  private def tell(msg: String): Writer[Log, Unit] =
    Writer.tell(Vector(msg))

  // лог удачного шага: откуда, куда, сколько стоило
  def logMove(from: Pos, dir: Direction, to: Pos, cost: Int): Writer[Log, Unit] =
    tell(s"[MOVE] $from -> ${dir.label} -> $to  (энергия: -$cost)")

  // лог отклонённого шага с причиной
  def logFailedMove(from: Pos, dir: Direction, reason: String): Writer[Log, Unit] =
    tell(s"[FAIL] попытка ${dir.label} из $from отклонена: $reason")

  // лог подбора предмета
  def logPick(pos: Pos, item: Item, cost: Int): Writer[Log, Unit] =
    tell(s"[PICK] подобран ${item.summary} на $pos  (энергия: -$cost)")

  // лог отказа в подборе
  def logPickFail(pos: Pos, reason: String): Writer[Log, Unit] =
    tell(s"[FAIL] подбор на $pos отклонён: $reason")

  // лог изменения энергии
  def logEnergy(before: Int, after: Int): Writer[Log, Unit] =
    tell(s"[NRG]  энергия: $before -> $after")

  // лог зарядки
  def logRecharge(amount: Int, after: Int): Writer[Log, Unit] =
    tell(s"[CHG]  зарядка +$amount, теперь $after")

  // лог сброса предмета
  def logDrop(pos: Pos, item: Item): Writer[Log, Unit] =
    tell(s"[DROP] брошен ${item.summary} на $pos")

  // лог отказа в сбросе
  val logDropFail: Writer[Log, Unit] =
    tell("[FAIL] нечего бросать — инвентарь пуст")
