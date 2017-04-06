package fsm.turnstile.data

import akka.actor.{ActorSystem, FSM, Props}
import fsm.turnstile.data.Commands._
import fsm.turnstile.data.Datas.StateData
import fsm.turnstile.data.States._

object States {

  sealed trait State

  case object Locked extends State

  case object UnLocked extends State

}

object Commands {

  sealed trait Command

  case class Coin(money: Int) extends Command

  case object Push extends Command

}

object Datas {

  case class StateData(money: Int)

}


class Turnstile extends FSM[State, StateData] {
  startWith(Locked, StateData(0))

  private val PRICE = 5

  when(Locked) {

    case Event(Push, _) =>
      println("<Locked!>")
      stay()

    case Event(Coin(money), StateData(total)) =>

      println(s"<Got Coin, money: $money, total: $total>")

      if (money + total >= PRICE) {

        goto(UnLocked) using StateData(money + total)

      } else {

        stay() using StateData(money + total)

      }

  }

  when(UnLocked) {

    case Event(Push, StateData(total)) =>

      println("<Come in>")

      if (total - PRICE > 0) {
        stay() using StateData(total - PRICE)
      } else {
        goto(Locked) using StateData(0)
      }

    case Event(Coin(money), StateData(total)) =>

      println(s"<Even more Coins, money: $money, total: $total>")

      stay() using StateData(money + total)
  }

  onTransition {
    case UnLocked -> Locked =>
      println("<Transition UnLocked -> Locked>")
    case _ =>
  }


  initialize()

}


object TurnstileDataMain extends App {


  def pause(): Unit = Thread.sleep(100)

  import Commands._

  val system = ActorSystem("FSM")

  val turnstile = system.actorOf(Props[Turnstile])

  println("Trying to push a Turnstile...")
  turnstile ! Push

  pause()

  println("...and put one Coin inside")

  turnstile ! Coin(1)

  pause()

  println("...and push it again")

  turnstile ! Push

  pause()


  println("...and put 4 Coins")

  turnstile ! Coin(4)

  pause()

  println("...and push it again")

  turnstile ! Push

  pause()

  println("...and push it again")

  turnstile ! Push

}
