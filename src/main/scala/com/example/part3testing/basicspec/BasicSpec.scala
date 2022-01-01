package com.example.part3testing.basicspec

import akka.actor.testkit.typed.scaladsl.ActorTestKit
import akka.actor.typed._
import akka.actor.typed.scaladsl.Behaviors
import org.scalatest.BeforeAndAfterAll
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import scala.concurrent.duration._
import scala.language.postfixOps

case class Ping(text: String, replyTo: ActorRef[String])

object SimpleActor {
  def apply(): Behavior[Ping] = Behaviors.receiveMessage { message =>
      message.replyTo ! message.text
      Behaviors.same
  }
}

object Blackhole {
  def apply(): Behavior[Ping] = Behaviors.empty
}

class BasicSpec
  extends AnyWordSpec
    with BeforeAndAfterAll
    with Matchers {
  val testKit = ActorTestKit()

  import SimpleActor._

  "A simple actor" should {
    "send back the same message" in {
      val echoActor = testKit.spawn(SimpleActor(), "ping")
      val probe = testKit.createTestProbe[String]()
      val text = "hello"

      echoActor ! Ping(text, probe.ref)

      probe.expectMessage(text)
    }

    "receiveN" in {
      val echoActor = testKit.spawn(SimpleActor(), "ping")
      val probe = testKit.createTestProbe[String]()
      val text = "hello"

      echoActor ! Ping(text, probe.ref)
      echoActor ! Ping(text, probe.ref)

      probe.receiveMessages(2)
    }
  }

  "A blackhole actor" should {
    "not send back any messages" in {
      val blackhole = testKit.spawn(Blackhole(), "blackhole")
      val probe = testKit.createTestProbe[String]()
      val text = "hello, test"
      blackhole ! Ping(text, probe.ref)

      probe.expectNoMessage(1 second)
    }
  }

  override def afterAll(): Unit = testKit.shutdownTestKit()
}
