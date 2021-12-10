package com.example.part2actors

import akka.actor.typed.scaladsl.Behaviors
import akka.actor.typed.{ActorRef, ActorSystem, Behavior}

object SimpleActor {
  sealed trait Command
  final case class Hi(replyTo: ActorRef[SimpleActor.Command]) extends Command
  final case class Message(message: String) extends Command
  final case class Number(number: Int) extends Command
  final case class SpecialMessage(contents: String) extends Command
  final case class SendMessageToYourself(content: String) extends Command
  final case class SayHiTo(ref: ActorRef[Hi], replyTo: ActorRef[SimpleActor.Command]) extends Command

  def apply(): Behavior[Command] = Behaviors.receive { (actorContext, message) =>
    message match {
      case Hi(replyTo) =>
        replyTo ! Message(s"Hello, there! (from ${actorContext.self.path.name})")
        Behaviors.same
      case Message(message) =>
        println(s"[${actorContext.self}] I have received $message")
        Behaviors.same
      case Number(number) =>
        println(s"[simple actor] I have received a NUMBER: $number")
        Behaviors.same
      case SpecialMessage(contents) =>
        println(s"[simple actor] I have received something SPECIAL: $contents")
        Behaviors.same
      case SendMessageToYourself(content) =>
        actorContext.self ! Message(content)
        Behaviors.same
      case SayHiTo(ref, replyTo) =>
        ref ! Hi(replyTo)
        Behaviors.same
    }
  }
}

object Counter {
  var count = 0

  sealed trait Command
  final case class Increment() extends Command
  final case class Decrement() extends Command
  final case class Print() extends Command

  def apply(): Behavior[Command] = Behaviors.receiveMessage {
    case Increment() =>
      count += 1
      Behaviors.same
    case Decrement() =>
      count -= 1
      Behaviors.same
    case Print() =>
      println(s"[counter] My current count is $count")
      Behaviors.same
  }
}

object ActorCapabilitiesMain {
  final case class SayHello()

  def apply(): Behavior[SayHello] =
    Behaviors.setup { context =>
      val simpleActor = context.spawn(SimpleActor(), "simpleActor")
      val alice = context.spawn(SimpleActor(), "alice")
      val bob = context.spawn(SimpleActor(), "bob")
      val counter = context.spawn(Counter(), "myCounter")

      Behaviors.receiveMessage { _ =>
        simpleActor ! SimpleActor.Message("hello, actor")
        simpleActor ! SimpleActor.Number(42)
        simpleActor ! SimpleActor.SpecialMessage("some special content")
        simpleActor ! SimpleActor.SendMessageToYourself("I am an actor and I am proud of it")
        simpleActor ! SimpleActor.SayHiTo(alice, bob)

        import Counter._
        (1 to 5).foreach(_ => counter ! Increment())
        (1 to 3).foreach(_ => counter ! Decrement())
        counter ! Print()

        Behaviors.same
      }
    }
}

object ActorCapabilities extends App {
  val actorCapabilitiesMain: ActorSystem[ActorCapabilitiesMain.SayHello] = ActorSystem(ActorCapabilitiesMain(), "ActorCapabilities")

  actorCapabilitiesMain ! ActorCapabilitiesMain.SayHello()
}

