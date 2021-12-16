package com.example.part2actors.changingactorbehavior

import akka.actor.typed.scaladsl.Behaviors
import akka.actor.typed.{ActorRef, ActorSystem, Behavior}

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

object Counter {
  sealed trait Command

  final case class Increment() extends Command

  final case class Decrement() extends Command

  final case class Print() extends Command

  def apply(): Behavior[Command] = countReceive(0)

  private def countReceive(currentCount: Int): Behavior[Command] =
    Behaviors.receiveMessage {
      case Increment() =>
        println(s"[countReceive($currentCount)] incrementing")
        countReceive(currentCount + 1)
      case Decrement() =>
        println(s"[countReceive($currentCount)] decrementing")
        countReceive(currentCount - 1)
      case Print() =>
        println(s"[counter] My current count is $currentCount")
        Behaviors.same
    }
}

sealed trait CitizenMessage

final case class Vote(candidate: String) extends CitizenMessage

final case class VoteStatusRequest(replyTo: ActorRef[VoteStatusReply]) extends CitizenMessage

sealed trait VoteAggregatorMessage

final case class VoteStatusReply(candidate: Option[String], replyTo: ActorRef[CitizenMessage]) extends VoteAggregatorMessage

final case class AggregateVotes(citizens: Set[ActorRef[CitizenMessage]]) extends VoteAggregatorMessage

object Citizen {
  def apply(): Behavior[CitizenMessage] = Behaviors.receive { (actorContext, message) =>
    message match {
      case Vote(c) => voted(c)
      case VoteStatusRequest(replyTo) =>
        replyTo ! VoteStatusReply(None, actorContext.self)
        Behaviors.same
    }
  }

  def voted(candidate: String): Behavior[CitizenMessage] = Behaviors.receive { (actorContext, message) =>
    message match {
      case VoteStatusRequest(replyTo) =>
        replyTo ! VoteStatusReply(Some(candidate), actorContext.self)
        Behaviors.same
    }
  }
}

object VoteAggregator {
  def apply(): Behavior[VoteAggregatorMessage] = Behaviors.receive { (actorContext, message) =>
    message match {
      case AggregateVotes(citizens) =>
        citizens.foreach(citizenRef => citizenRef ! VoteStatusRequest(actorContext.self))
        awaitingStatuses(citizens, Map())
    }
  }

  def awaitingStatuses(stillWaiting: Set[ActorRef[CitizenMessage]], currentStats: Map[String, Int]): Behavior[
    VoteAggregatorMessage] = Behaviors.receive { (actorContext, message) =>
    message match {
      case VoteStatusReply(None, replyTo) =>
        replyTo ! VoteStatusRequest(actorContext.self) // this might end up in an infinite loop
        Behaviors.same
      case VoteStatusReply(Some(candidate), replyTo) =>
        val newStillWaiting = stillWaiting - replyTo
        val currentVotesOfCandidate = currentStats.getOrElse(candidate, 0)
        val newStats = currentStats + (candidate -> (currentVotesOfCandidate + 1))
        if (newStillWaiting.isEmpty) {
          println(s"[aggregator] poll stats: $newStats")
          Behaviors.same
        } else {
          // still need to process some statuses
          awaitingStatuses(newStillWaiting, newStats)
        }
    }
  }
}

object ChangingActorBehaviorMain {
  final case class SayHello()

  def apply(): Behavior[SayHello] =
    Behaviors.setup { context =>
      val statelessFussyKid = context.spawn(StatelessFussyKid(), "statelessFussyKid")
      val mom = context.spawn(Mom(), "mom")
      val counter = context.spawn(Counter(), "myCounter")
      val alice = context.spawn(Citizen(), "alice")
      val bob = context.spawn(Citizen(), "bob")
      val charlie = context.spawn(Citizen(), "charlie")
      val daniel = context.spawn(Citizen(), "daniel")
      val voteAggregator = context.spawn(VoteAggregator(), "voteAggregator")

      Behaviors.receiveMessage { _ =>
        mom ! Mom.MomStart(statelessFussyKid)

        import Counter._
        (1 to 5).foreach(_ => counter ! Increment())
        (1 to 3).foreach(_ => counter ! Decrement())
        counter ! Print()

        alice ! Vote("Martin")
        bob ! Vote("Jonas")
        charlie ! Vote("Roland")
        daniel ! Vote("Roland")

        voteAggregator ! AggregateVotes(Set(alice, bob, charlie, daniel))

        Behaviors.same
      }
    }
}

object ChangingActorBehavior extends App {
  val changingActorBehaviorMain: ActorSystem[ChangingActorBehaviorMain.SayHello] = ActorSystem(ChangingActorBehaviorMain(), "ChangingActorBehavior")

  changingActorBehaviorMain ! ChangingActorBehaviorMain.SayHello()
}
