//#full-example
package com.example


import akka.actor.typed.{ActorSystem, Behavior}
import akka.actor.typed.scaladsl.Behaviors
import com.example.GreeterMain.SayHello

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

//#greeter-main
object GreeterMain {

  final case class SayHello()

  def apply(): Behavior[SayHello] =
    Behaviors.setup { context =>
      val wordCount = context.spawn(WordCountActor(), "wordCount")

      Behaviors.receiveMessage { message =>
        //#create-actors
        wordCount ! WordCountActor.CountWords("I am learning Akka and it's pretty damn cool!")
        Behaviors.same
      }
    }
}

object AkkaQuickstart extends App {
  val greeterMain: ActorSystem[GreeterMain.SayHello] = ActorSystem(GreeterMain(), "AkkaQuickStart")

  greeterMain ! SayHello()
}
