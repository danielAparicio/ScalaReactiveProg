package kvstore

import akka.actor.Props
import akka.actor.Actor
import akka.actor.ActorRef
import scala.language.postfixOps
import scala.concurrent.duration._



object Replicator {
  case class Replicate(key: String, valueOption: Option[String], id: Long)
  case class Replicated(key: String, id: Long)
  
  case class Snapshot(key: String, valueOption: Option[String], seq: Long)
  case class SnapshotAck(key: String, seq: Long)

  case object RetrySnap
  
  def props(replica: ActorRef): Props = Props(new Replicator(replica))
}

class Replicator(val replica: ActorRef) extends Actor {
  import Replicator._
  import Replica._
  import context.dispatcher
  
  //retry all not-Acked every 100 millis
  context.system.scheduler.schedule(100 millis, 100 millis, context.self, RetrySnap)

  // map [sequence number , pair sender and request ]
  var acks = Map.empty[Long, (ActorRef, Replicate)]
  // a sequence of not-yet-sent snapshots (you can disregard this if not implementing batching)
  var pending = Vector.empty[Snapshot]
  
  var seqCounter = 0L
  def nextSeq = {
    val ret = seqCounter
    seqCounter += 1
    ret
  }
  
  def receive: Receive = {
    case rep @ Replicate(key, valueOption, id) => {
      val seq = nextSeq
      acks += seq -> (sender, rep) 
      replica ! Snapshot(key, valueOption, seq)
    }
    case SnapshotAck(key, seq) => {
      acks.get(seq).map{entry =>
        val (primary, command) = entry
        primary ! Replicated(key, command.id)
      }
      acks -= seq 
    }
    case RetrySnap => {
      acks.foreach(entry => {
        val (seq, (primary, replicate)) = entry
        replica ! Snapshot(replicate.key, replicate.valueOption, seq)
      })
    }
  }

}
