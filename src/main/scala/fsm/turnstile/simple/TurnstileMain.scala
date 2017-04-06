package fsm.turnstile.simple

import akka.actor.{ActorSystem, FSM, Props}
import fsm.turnstile.simple.Commands._
import fsm.turnstile.simple.States._

object States {

  sealed trait State

  case object Locked extends State

  case object UnLocked extends State

}

object Commands {

  sealed trait Command

  case object Coin extends Command

  case object Push extends Command

}

class Turnstile extends FSM[State, Option[Int]] {
  startWith(Locked, None)

  when(Locked) {
    case Event(Push, _) =>
      println("<Locked!>")
      stay()

    case Event(Coin, _) =>

      println(s"<Got Coin>")

      goto(UnLocked)
  }

  when(UnLocked) {
    case Event(Push, _) =>
      println("<Come in>")

      goto(Locked)

    case Event(Coin, _) =>

      println(s"<Even more Coins>")

      stay()
  }


  initialize()
}


object TurnstileMain extends App {


  def pause(): Unit = Thread.sleep(100)

  val system = ActorSystem("FSM")

  val turnstile = system.actorOf(Props[Turnstile])

  println("Trying to push a Turnstile...")
  turnstile ! Push

  pause()

  println("...and put a Coin inside")

  turnstile ! Coin

  pause()

  println("...and push it again")

  turnstile ! Push

  pause()

  println("...and push it two times")

  turnstile ! Push
  turnstile ! Push

  pause()

  println("...and put two Coins")

  turnstile ! Coin
  turnstile ! Coin

}
