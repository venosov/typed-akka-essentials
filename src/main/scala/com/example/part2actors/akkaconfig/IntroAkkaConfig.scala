package com.example.part2actors.akkaconfig

import akka.actor.typed.scaladsl.Behaviors
import akka.actor.typed.{ActorSystem, Behavior}

object Person {
  def apply(name: String): Behavior[String] = Behaviors.receive { (context, message) =>
    context.log.debug("Hello {}!", message)
    Behaviors.same
  }
}

object IntroAkkaConfigMain {
  final case class SayHello()

  def apply(): Behavior[SayHello] =
    Behaviors.setup { context =>
      val person = context.spawn(Person("Bob"), "person")

      Behaviors.receiveMessage { _ =>
        person ! "world"
        Behaviors.same
      }
    }
}

object IntroAkkaConfig extends App {
  val introAkkaConfigMain: ActorSystem[IntroAkkaConfigMain.SayHello] = ActorSystem(IntroAkkaConfigMain(), "IntroAkkaConfig")

  introAkkaConfigMain ! IntroAkkaConfigMain.SayHello()
}

