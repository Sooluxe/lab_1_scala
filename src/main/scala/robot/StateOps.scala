package robot

import monads.State

// чистые переходы состояния. State[RobotState, A] — это функция
// RobotState => (RobotState, A): берёт состояние, возвращает новое + результат.
object StateOps:

  // двигает робота в позицию to, отнимает cost. возвращает новую позицию.
  def move(to: Pos, cost: Int): State[RobotState, Pos] =
    State { st =>
      val newSt = st.copy(pos = to, energy = st.energy - cost)
      (newSt, to)
    }

  // подбирает предмет с текущей клетки. Some(item) если был, None если нет.
  // Map.get отдаёт Option[Item]; "Map - key" возвращает новый Map без этого ключа.
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

  // выбрасывает последний собранный предмет на текущую клетку.
  // dropRight(1) убирает последний элемент; Map + (k -> v) добавляет пару.
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

  // заряжает до максимума. возвращает, на сколько прибавилось.
  def recharge(maxEnergy: Int): State[RobotState, Int] =
    State { st =>
      val gained = maxEnergy - st.energy
      (st.copy(energy = maxEnergy), gained)
    }
