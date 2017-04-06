package fsm.turnstile.timeout

import akka.actor.{ActorSystem, FSM, Props}
import fsm.turnstile.timeout.Commands._
import fsm.turnstile.timeout.States._

import scala.concurrent.duration._

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

  private val TIMEOUT = 5 seconds

  when(Locked) {
    case Event(Push, _) =>
      println("<Locked!>")
      stay()

    case Event(Coin, _) =>

      println(s"<Got Coin>")

      goto(UnLocked) forMax TIMEOUT

  }

  when(UnLocked) {
    case Event(Push, _) =>
      println("<Come in>")

      goto(Locked)

    case Event(Coin, _) =>

      println(s"<Even more Coins>")

      stay()

    case Event(StateTimeout, _) =>

      println("<Going to Locked after timeout>")

      goto(Locked)
  }


  initialize()
}


object TurnstileTimeoutMain extends App {


  def pause(): Unit = Thread.sleep(100)

  val system = ActorSystem("FSM")

  val turnstile = system.actorOf(Props[Turnstile])

  println("Trying to push a Turnstile...")

  turnstile ! Push

  pause()

  println("...and put a Coin inside")

  turnstile ! Coin

  pause()

  println("...and wait for 6 seconds...")

  Thread.sleep(6000)

  println("...and push it")

  turnstile ! Push

  pause()

  println("...and put a Coin inside")

  turnstile ! Coin

  pause()

  println("...and push it again")

  turnstile ! Push


}
