package robot

import monads.{Writer, Monoid}
import monads.Monoid.given

object WriterOps:

  private def tell(msg: String): Writer[Log, Unit] =
    Writer.tell(Vector(msg))

  def logMove(from: Pos, dir: Direction, to: Pos, cost: Int): Writer[Log, Unit] =
    tell(s"[MOVE] $from -> ${dir.label} -> $to  (энергия: -$cost)")

  def logFailedMove(from: Pos, dir: Direction, reason: String): Writer[Log, Unit] =
    tell(s"[FAIL] попытка ${dir.label} из $from отклонена: $reason")

  def logPick(pos: Pos, item: Item, cost: Int): Writer[Log, Unit] =
    tell(s"[PICK] подобран ${item.summary} на $pos  (энергия: -$cost)")

  def logPickFail(pos: Pos, reason: String): Writer[Log, Unit] =
    tell(s"[FAIL] подбор на $pos отклонён: $reason")

  def logEnergy(before: Int, after: Int): Writer[Log, Unit] =
    tell(s"[NRG]  энергия: $before -> $after")

  def logRecharge(amount: Int, after: Int): Writer[Log, Unit] =
    tell(s"[CHG]  зарядка +$amount, теперь $after")

  def logDrop(pos: Pos, item: Item): Writer[Log, Unit] =
    tell(s"[DROP] брошен ${item.summary} на $pos")

  val logDropFail: Writer[Log, Unit] =
    tell("[FAIL] нечего бросать — инвентарь пуст")
