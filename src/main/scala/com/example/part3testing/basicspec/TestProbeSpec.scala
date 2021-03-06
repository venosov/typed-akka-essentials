package com.example.part3testing.basicspec

import akka.actor.testkit.typed.scaladsl.ActorTestKit
import akka.actor.typed.scaladsl.Behaviors
import akka.actor.typed.{ActorRef, Behavior}
import org.scalatest.BeforeAndAfterAll
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

final case class SlaveWork(text: String, originalRequester: ActorRef[Report])

sealed trait SenderMessages

final case class RegistrationAck() extends SenderMessages

case class Report(totalCount: Int) extends SenderMessages

sealed trait MasterMessages

final case class Register(sender: ActorRef[RegistrationAck], slaveRef: ActorRef[SlaveWork]) extends MasterMessages

final case class Work(sender: ActorRef[Report], text: String) extends MasterMessages

final case class WorkCompleted(count: Int, originalRequester: ActorRef[Report]) extends MasterMessages


class TestProbeSpec
  extends AnyWordSpec
    with BeforeAndAfterAll
    with Matchers {
  private val testKit = ActorTestKit()

  override def afterAll(): Unit = testKit.shutdownTestKit()

  "A master actor" should {
    "register a slave" in {
      val master = testKit.spawn(Master(), "master")
      val sender = testKit.createTestProbe[SenderMessages]()
      val slave = testKit.createTestProbe[SlaveWork]()

      master ! Register(sender.ref, slave.ref)

      sender.receiveMessage()
    }

    "send the work to the slave actor" in {
      val master = testKit.spawn(Master(), "master2")
      val sender = testKit.createTestProbe[SenderMessages]()
      val slave = testKit.createTestProbe[SlaveWork]()
      val mockedBehavior = Behaviors.receiveMessage[SlaveWork] { msg =>
        msg.originalRequester ! Report(3)
        Behaviors.same
      }
      val mockedSlave = testKit.spawn(Behaviors.monitor(slave.ref, mockedBehavior))

      master ! Register(sender.ref, mockedSlave)
      val workloadString = "I love Akka"
      master ! Work(sender.ref, workloadString)

      sender.receiveMessage()
      slave.expectMessage(SlaveWork(workloadString, sender.ref))
      sender.expectMessage(Report(3))
    }
  }
}

object Master {
  def apply(): Behavior[MasterMessages] = Behaviors.receiveMessage {
    case Register(sender, slaveRef) =>
      sender ! RegistrationAck()
      online(slaveRef, 0)
  }

  def online(slaveRef: ActorRef[SlaveWork], totalWordCount: Int): Behavior[MasterMessages] = Behaviors.receiveMessage {
    case Work(sender, text) =>
      slaveRef ! SlaveWork(text, sender)
      Behaviors.same
    case WorkCompleted(count, originalRequester) =>
      val newTotalWordCount = totalWordCount + count
      originalRequester ! Report(newTotalWordCount)
      online(slaveRef, newTotalWordCount)
  }
}
