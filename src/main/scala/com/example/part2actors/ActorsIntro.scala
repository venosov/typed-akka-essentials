package com.example.part2actors

import akka.actor.typed.{ActorSystem, Behavior}
import akka.actor.typed.scaladsl.Behaviors
import com.example.part2actors.GreeterMain.SayHello

object WordCountActor {
  // internal data
  var totalWords = 0

  // actor protocol
  sealed trait Command
  final case class CountWords(text: String) extends Command

  def apply(): Behavior[CountWords] = Behaviors.receive { (_, message) =>
    println(s"[word counter] I have received: ${message.text}")
    totalWords += message.text.split(" ").length
    Behaviors.same
  }
}

object Person {
  def apply(name: String): Behavior[String] = Behaviors.receive { (_, _) =>
    println(s"Hi, my name is $name")
    Behaviors.same
  }
}

//#greeter-main
object GreeterMain {
  final case class SayHello()

  def apply(): Behavior[SayHello] =
    Behaviors.setup { context =>
      val wordCount = context.spawn(WordCountActor(), "wordCount")
      val person = context.spawn(Person("Bob"), "person")

      Behaviors.receiveMessage { _ =>
        //#create-actors
        wordCount ! WordCountActor.CountWords("I am learning Akka and it's pretty damn cool!")
        person ! "hi"
        Behaviors.same
      }
    }
}

object ActorsIntro extends App {
  val greeterMain: ActorSystem[GreeterMain.SayHello] = ActorSystem(GreeterMain(), "ActorsIntro")

  greeterMain ! SayHello()
}

