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

object BankAccount {
  var funds = 0

  sealed trait Command

  final case class Deposit(amount: Int, replyto: ActorRef[Human.Command]) extends Command

  final case class Withdraw(amount: Int, replyto: ActorRef[Human.Command]) extends Command

  final case class Statement(replyto: ActorRef[Human.Command]) extends Command

  def apply(): Behavior[Command] = Behaviors.receiveMessage {
    case Deposit(amount, replyTo) =>
      if (amount < 0) replyTo ! Human.TransactionFailure("invalid deposit amount")
      else {
        funds += amount
        replyTo ! Human.TransactionSuccess(s"successfully deposited $amount")
      }
      Behaviors.same
    case Withdraw(amount, replyTo) =>
      if (amount < 0) replyTo ! Human.TransactionFailure("invalid withdrew amount")
      else if (amount > funds) replyTo ! Human.TransactionFailure("insufficient funds")
      else {
        funds -= amount
        replyTo ! Human.TransactionSuccess(s"successfully withdrew $amount")
      }
      Behaviors.same
    case Statement(replyTo) =>
      replyTo ! Human.Message(s"Your balance is $funds")
      Behaviors.same
  }
}

object Human {
  sealed trait Command

  final case class LiveTheLife(account: ActorRef[BankAccount.Command]) extends Command

  final case class TransactionSuccess(message: String) extends Command

  final case class TransactionFailure(reason: String) extends Command

  final case class Message(message: String) extends Command

  def apply(): Behavior[Command] = Behaviors.receive { (actorContext, message) =>
    message match {
      case LiveTheLife(account) =>
        account ! BankAccount.Deposit(10000, actorContext.self)
        account ! BankAccount.Withdraw(90000, actorContext.self)
        account ! BankAccount.Withdraw(500, actorContext.self)
        account ! BankAccount.Statement(actorContext.self)
        Behaviors.same
      case _ =>
        println(message.toString)
        Behaviors.same
    }
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
      val account = context.spawn(BankAccount(), "bankAccount")
      val person = context.spawn(Human(), "billionaraire")

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

        person ! Human.LiveTheLife(account)

        Behaviors.same
      }
    }
}

object ActorCapabilities extends App {
  val actorCapabilitiesMain: ActorSystem[ActorCapabilitiesMain.SayHello] = ActorSystem(ActorCapabilitiesMain(), "ActorCapabilities")

  actorCapabilitiesMain ! ActorCapabilitiesMain.SayHello()
}

