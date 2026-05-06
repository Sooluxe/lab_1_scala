package robot

import monads.State

object StateOps:

  def move(to: Pos, cost: Int): State[RobotState, Pos] =
    State { st =>
      val newSt = st.copy(pos = to, energy = st.energy - cost)
      (newSt, to)
    }

  def pickItem(cost: Int): State[RobotState, Option[Item]] =
    State { st =>
      st.itemsOnMap.get(st.pos) match
        case None =>
          (st, None)
        case Some(item) =>
          val newSt = st.copy(
            energy     = st.energy - cost,
            collected  = st.collected :+ item,
            itemsOnMap = st.itemsOnMap - st.pos
          )
          (newSt, Some(item))
    }

  val dropItem: State[RobotState, Option[Item]] =
    State { st =>
      st.collected.lastOption match
        case None =>
          (st, None)
        case Some(item) =>
          val newSt = st.copy(
            collected  = st.collected.dropRight(1),
            itemsOnMap = st.itemsOnMap + (st.pos -> item)
          )
          (newSt, Some(item))
    }

  def recharge(maxEnergy: Int): State[RobotState, Int] =
    State { st =>
      val gained = maxEnergy - st.energy
      (st.copy(energy = maxEnergy), gained)
    }
