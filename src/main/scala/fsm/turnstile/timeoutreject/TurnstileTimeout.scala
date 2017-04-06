package fsm.turnstile.timeoutreject

import akka.actor.{ActorSystem, FSM, Props}
import fsm.turnstile.timeoutreject.Commands._
import fsm.turnstile.timeoutreject.States._

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

  case object LookAt extends Command

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

    case Event(LookAt, _) =>
      println("<Do nothing>")
      stay()
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

    case Event(LookAt, _) =>
      println("<Do nothing>")
      stay()

  }


  initialize()
}


object TurnstileTimeoutRejectMain extends App {


  def pause(duration: Duration = 100 milliseconds): Unit = Thread.sleep(duration.toMillis)

  val system = ActorSystem("FSM")

  val turnstile = system.actorOf(Props[Turnstile])

  println("Trying to push a Turnstile...")

  turnstile ! Push

  pause()

  println("...and put a Coin inside")

  turnstile ! Coin

  pause()

  println("...and wait for 3 seconds...")

  pause(3 seconds)

  println("...do neutral event before timeout..")

  turnstile ! LookAt

  pause(3 seconds)

  println("...and push it after total 6 seconds")

  turnstile ! Push

  pause()

  println("...so no Locking after timeout!")

}
