package com.example.part2actors.childactors

import akka.actor.typed.{ActorRef, ActorSystem, Behavior}
import akka.actor.typed.scaladsl.Behaviors

object Child {
  def apply(): Behavior[String] = Behaviors.receive { (actorContext, message) =>
    println(s"${actorContext.self.path} I got: $message")
    Behaviors.same
  }
}

object Parent {
  sealed trait ParentMessages
  final case class CreateChild(name: String) extends ParentMessages
  final case class TellChild(message: String) extends ParentMessages

  def apply(): Behavior[ParentMessages] = Behaviors.receive { (actorContext, message) =>
    message match {
      case CreateChild(name) =>
        println(s"${actorContext.self.path} creating child")
        val childRef = actorContext.spawn(Child(), name)
        withChild(childRef)
    }
  }

  def withChild(childRef: ActorRef[String]): Behavior[ParentMessages] = Behaviors.receiveMessage {
      case TellChild(message) =>
        childRef ! message
        Behaviors.same
  }
}

object ChildActorsMain {
  final case class SayHello()

  import Parent._

  def apply(): Behavior[SayHello] =
    Behaviors.setup { context =>
      val parent = context.spawn(Parent(), "parent")

      Behaviors.receiveMessage { _ =>
        parent ! CreateChild("child")
        parent ! TellChild("hey Kid!")

        Behaviors.same
      }
    }
}

object ChildActors extends App {
  val childActorsMain: ActorSystem[ChildActorsMain.SayHello] = ActorSystem(ChildActorsMain(), "ChildActors")

  childActorsMain ! ChildActorsMain.SayHello()
}
