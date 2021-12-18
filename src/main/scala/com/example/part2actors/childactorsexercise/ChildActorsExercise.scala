package com.example.part2actors.childactorsexercise

import akka.actor.typed.{ActorRef, ActorSystem, Behavior}
import akka.actor.typed.scaladsl.Behaviors

object TestActor {
  import WordCounterMaster._

  sealed trait TestActorMessages
  final case object Go extends TestActorMessages
  final case class Count(count: Int) extends TestActorMessages

  def apply(): Behavior[TestActorMessages] = Behaviors.receive { (actorContext, message) =>
    message match {
      case Go =>
        val master = actorContext.spawn(WordCounterMaster(), "master")
        master ! Initialize(3)
        val texts = List("I love Akka", "Scala is super dope", "yes", "me too")
        texts.foreach(text => master ! Text(text, actorContext.self))
        Behaviors.same
      case Count(count) =>
        println(s"[test actor] I received a reply: $count")
        Behaviors.same
    }
  }
}

object WordCounterWorker {
  import WordCounterMaster._

  final case class WordCountTask(id: Int, text: String, replyTo: ActorRef[WordCountReply])

  def apply(): Behavior[WordCountTask] = Behaviors.receive { (actorContext, message) =>
    println(s"${actorContext.self.path} I have received task ${message.id} with ${message.text}")
    message.replyTo ! WordCountReply(message.id, message.text.split(" ").length)
    Behaviors.same
  }
}

object WordCounterMaster {
  import WordCounterWorker._
  import TestActor._

  sealed trait WordCounterMasterMessages
  final case class Initialize(nChildren: Int) extends WordCounterMasterMessages
  final case class WordCountReply(id: Int, count: Int) extends WordCounterMasterMessages
  final case class Text(text: String, replyTo: ActorRef[TestActorMessages]) extends WordCounterMasterMessages

  def apply(): Behavior[WordCounterMasterMessages] = Behaviors.receive { (actorContext, message) =>
    message match {
      case Initialize(nChildren) =>
        println("[master] initializing...")
        val childrenRefs = for (i <- 0 until nChildren) yield actorContext.spawn(WordCounterWorker(), s"wcw_$i")
        withChildren(childrenRefs, 0, 0, Map())
    }
  }

  def withChildren(childrenRefs: Seq[ActorRef[WordCountTask]], currentChildIndex: Int, currentTaskId: Int, requestMap:
  Map[Int, ActorRef[Count]]): Behavior[WordCounterMasterMessages] = Behaviors.receive { (actorContext, message) =>
    message match {
      case Text(text, replyTo) =>
        println(s"[master] I have received: $text - I will send it to child $currentChildIndex")
        val originalSender = replyTo
        val task = WordCountTask(currentTaskId, text, actorContext.self)
        val childRef = childrenRefs(currentChildIndex)
        childRef ! task
        val nextChildIndex = (currentChildIndex + 1) % childrenRefs.length
        val newTaskId = currentTaskId + 1
        val newRequestMap = requestMap + (currentTaskId -> originalSender)
        withChildren(childrenRefs, nextChildIndex, newTaskId, newRequestMap)
      case WordCountReply(id, count) =>
        println(s"[master] I have received a reply for task id $id with $count")
        val originalSender = requestMap(id)
        originalSender ! Count(count)
        withChildren(childrenRefs, currentChildIndex, currentTaskId, requestMap - id)
    }
  }
}

object ChildActorsMainExercise {
  final case class SayHello()


  def apply(): Behavior[SayHello] =
    Behaviors.setup { context =>
      val testActor = context.spawn(TestActor(), "testActor")

      Behaviors.receiveMessage { _ =>
        testActor ! TestActor.Go
        Behaviors.same
      }
    }
}

object ChildActorsExercise extends App {
  val childActorsMain: ActorSystem[ChildActorsMainExercise.SayHello] = ActorSystem(ChildActorsMainExercise(), "ChildActors")

  childActorsMain ! ChildActorsMainExercise.SayHello()
}
