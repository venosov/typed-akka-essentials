package com.example.part2actors.actorlogging

import akka.actor.typed.scaladsl.Behaviors
import akka.actor.typed.{ActorRef, ActorSystem, Behavior}

object ActorWithLogging {
  def apply(): Behavior[String] = Behaviors.receive { (context, message) =>
    context.log.info("Hello {}!", message)
    Behaviors.same
  }
}

object ActorLoggingDemoMain {
  final case class SayHello()

  def apply(): Behavior[SayHello] =
    Behaviors.setup { context =>
      val actorWithLogging = context.spawn(ActorWithLogging(), "actorWithLogging")
      Behaviors.receiveMessage { _ =>
        actorWithLogging ! "world"
        Behaviors.same
      }
    }
}

object ActorLoggingDemo extends App {
  val actorLoggingDemoMain: ActorSystem[ActorLoggingDemoMain.SayHello] = ActorSystem(ActorLoggingDemoMain(), "ActorLoggingDemo")

  actorLoggingDemoMain ! ActorLoggingDemoMain.SayHello()
}

