package com.example.part2actors

import akka.actor.typed.{ActorSystem, Behavior}
import akka.actor.typed.scaladsl.Behaviors
import akka.actor.typed.ActorRef

object StatelessFussyKid {
  import Mom._

  val VEGETABLE = "veggies"
  val CHOCOLATE = "chocolate"

  sealed trait Command
  final case class Food(food: String) extends Command
  final case class Ask(message: String, replyTo: ActorRef[KidAnswer]) extends Command // do you want to play?

  def apply(): Behavior[Command] = happyReceive()

  private def happyReceive(): Behavior[Command] =
    Behaviors.receiveMessage {
      case Food(VEGETABLE) => sadReceive()
      case Food(CHOCOLATE) => Behaviors.same
      case Ask(_, replyTo) =>
        replyTo ! KidAccept
        Behaviors.same
    }

  private def sadReceive(): Behavior[Command] =
    Behaviors.receiveMessage {
      case Food(VEGETABLE) => Behaviors.same
      case Food(CHOCOLATE) => happyReceive()
      case Ask(_, replyTo) =>
        replyTo ! KidReject
        Behaviors.same
    }
}

object Mom {
  import StatelessFussyKid._
  sealed trait MomMessage
  final case class MomStart(kidRef: ActorRef[Command]) extends MomMessage
  sealed trait KidAnswer extends MomMessage
  case object KidAccept extends KidAnswer()
  case object KidReject extends KidAnswer()

  def apply(): Behavior[MomMessage] = Behaviors.receive { (actorContext, message) =>
    message match {
      case MomStart(kidRef) =>
        kidRef ! Ask("do you want to play?", actorContext.self)
        kidRef ! Food(VEGETABLE)
        kidRef ! Ask("do you want to play?", actorContext.self)
        Behaviors.same
      case KidAccept =>
        println("Yay, my kid is happy!")
        Behaviors.same
      case KidReject =>
        println("My kid is sad, but as he's healthy!")
        Behaviors.same
    }
  }
}

object ChangingActorBehaviorMain {
  final case class SayHello()

  def apply(): Behavior[SayHello] =
    Behaviors.setup { context =>
      val statelessFussyKid = context.spawn(StatelessFussyKid(), "statelessFussyKid")
      val mom = context.spawn(Mom(), "mom")

      Behaviors.receiveMessage { _ =>
        mom ! Mom.MomStart(statelessFussyKid)
        Behaviors.same
      }
    }
}

object ChangingActorBehavior extends App {
  val changingActorBehaviorMain : ActorSystem[ChangingActorBehaviorMain.SayHello] = ActorSystem(ChangingActorBehaviorMain(), "ChangingActorBehavior")

  changingActorBehaviorMain ! ChangingActorBehaviorMain.SayHello()
}
