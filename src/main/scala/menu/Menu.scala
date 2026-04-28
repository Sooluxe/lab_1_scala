package menu

import monads.*
import domain.*
import domain.StateOps.Result

// что делает пункт меню — четыре варианта на все случаи жизни
enum MenuAction:
  case Run(t: State[RobotState, Result]) // запустить переход состояния
  case Goto(sub: Menu)                    // войти в подменю (вложенность)
  case Back                               // вернуться в родительское меню
  case Quit                               // выйти из программы

// один пункт меню: что показать + что делать. номер сам пристрелится по индексу
final case class MenuItem(label: String, action: MenuAction)

// само меню — заголовок и список пунктов. через Goto получается дерево
final case class Menu(title: String, items: List[MenuItem])
