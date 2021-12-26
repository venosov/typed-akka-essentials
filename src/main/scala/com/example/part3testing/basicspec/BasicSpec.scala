package com.example.part3testing.basicspec

import akka.actor.testkit.typed.scaladsl.ActorTestKit
import akka.actor.typed._
import akka.actor.typed.scaladsl.Behaviors
import org.scalatest.BeforeAndAfterAll
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

object SimpleActor {
  case class Ping(text: String, replyTo: ActorRef[String])

  def apply(): Behavior[Ping] = Behaviors.receiveMessage { message =>
      message.replyTo ! message.text
      Behaviors.same
  }
}

class BasicSpec
  extends AnyWordSpec
    with BeforeAndAfterAll
    with Matchers {
  val testKit = ActorTestKit()

  import SimpleActor._

  "A simple actor" must {
    "send back the same message" in {
      val echoActor = testKit.spawn(SimpleActor(), "ping")
      val probe = testKit.createTestProbe[String]()
      val text = "hello"

      echoActor ! Ping(text, probe.ref)

      probe.expectMessage(text)
    }
  }

  override def afterAll(): Unit = testKit.shutdownTestKit()
}
