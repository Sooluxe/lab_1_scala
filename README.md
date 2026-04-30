# ФП Лаба 1 — Вариант 3: Робот на сетке

## Структура проекта

```
src/main/scala/
  monads/
    Monad.scala       — typeclass Monad с extension-методами
    IO.scala          — IO + given ioMonad
    Reader.scala      — Reader + given readerMonad
    State.scala       — State + given stateMonad
    Writer.scala      — Writer, Monoid + given writerMonad
  robot/
    Domain.scala      — Pos, Direction, Item, Config, RobotState,
                        trait MenuOption, trait UserInteraction
    Menu.scala        — MenuLeaf, MenuTreeNode (автонумерация, рекурсия по дереву)
    ReaderOps.scala   — canMove, energyCost, nextPos и т.п.
    StateOps.scala    — move, pickItem, dropItem, recharge
    WriterOps.scala   — logMove, logFailedMove, logPick, logEnergy, logRecharge, logDrop
    App.scala         — flows + buildMenu + program + main
```

## Запуск

Нужны установленные `sbt` и Java 11+.

```bash
# из корня проекта
sbt run
```

Если sbt ещё нет:

```bash
# macOS
brew install sbt
```

Полезное:

```bash
sbt compile     # собрать без запуска
sbt clean       # снести target/
sbt console     # REPL с подгруженным проектом
```

После `sbt run` появится главное меню — управление цифрами, `0` — выход
из подменю или из программы.

## Меню

Главное меню — дерево `MenuTreeNode`. Заголовок переcобирается каждый цикл и
показывает живое состояние робота: позицию, энергию, число собранных предметов.

```
=== Робот: pos=(0,0) | энергия=30 | собрано=0 ===
  1) Движение         → подменю (Вверх / Вниз / Влево / Вправо)
  2) Предметы         → подменю (Подобрать / Бросить)
  3) Зарядка
  4) Показать карту
  0) Выход
```

Добавить новый пункт = вписать новый `MenuLeaf` в `children` соответствующего
узла. Никакого `case "1" / case "2"` — нумерация автоматическая через
`zipWithIndex`.

## Карта

```
. * . . .    (0,0) — старт робота R
. . . . .    *     — предметы: ключ(1,0), монета(3,1), кристалл(0,4)
. . X . .    X     — стены: (2,2) и (1,3)
. X . . .
* . . . .
```
