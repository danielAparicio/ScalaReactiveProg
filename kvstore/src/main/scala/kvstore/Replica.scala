package kvstore

import akka.actor.{ OneForOneStrategy, Props, ActorRef, Actor }
import akka.actor.PoisonPill
import akka.actor.OneForOneStrategy
import akka.actor.SupervisorStrategy
import akka.actor.AllForOneStrategy
import akka.actor.ActorKilledException
import akka.actor.Terminated
import akka.actor.SupervisorStrategy.{Restart, Stop}
import kvstore.Arbiter._
import scala.collection.immutable.Queue
import scala.annotation.tailrec
import akka.pattern.{ ask, pipe }
import scala.concurrent.duration._
import akka.util.Timeout
import scala.language.postfixOps
import akka.dispatch.Foreach



object Replica {
  sealed trait Operation {
    def key: String
    def id: Long
  }
  case object RetryPersist
  case class TimeOut(id: Long)
  
  case class Insert(key: String, value: String, id: Long) extends Operation
  case class Remove(key: String, id: Long) extends Operation
  case class Get(key: String, id: Long) extends Operation

  sealed trait OperationReply
  case class OperationAck(id: Long) extends OperationReply
  case class OperationFailed(id: Long) extends OperationReply
  case class GetResult(key: String, valueOption: Option[String], id: Long) extends OperationReply

  def props(arbiter: ActorRef, persistenceProps: Props): Props = Props(new Replica(arbiter, persistenceProps))
}

class Replica(val arbiter: ActorRef, persistenceProps: Props) extends Actor {
  import Replica._
  import Replicator._
  import Persistence._
  import context.dispatcher

  context.system.scheduler.schedule(100 millis, 100 millis, context.self, RetryPersist)
  
  override def supervisorStrategy = AllForOneStrategy(){
    case _:PersistenceException => Restart
    case _:ActorKilledException => Stop 
  }
  
  val beforeStart = arbiter ! Join
    
  val persistence = context.actorOf(persistenceProps)
  
  var kv = Map.empty[String, String]
  //map[secondary replicas to replicators]
  var secondary = Map.empty[ActorRef, ActorRef]
  //current set of the replicators
  var replicators = Set.empty[ActorRef]
  //persisting the list that hasn't been Acked
  var notAcked = Map.empty[Long, (ActorRef, Option[Persist], Set[ActorRef])]
  
  var expectedVersion = 0L
  var _seqCounter = 0L
  def nextSeq = {
    val ret = _seqCounter
    _seqCounter += 1
    ret
  }

  def receive = {
    case JoinedPrimary   => context.become(leader)
    case JoinedSecondary => context.become(replica)
  }

  val leader: Receive = {
    case Get(key, id) => sender ! GetResult(key, kv.get(key), id)
    case RetryPersist => {
      notAcked.foreach(entry => {
        val (_, (_, command, _)) = entry
        command.map(persistence ! _)
      })
    }
    
    case Insert(key, value, id) => {
      kv += key -> value
      persistPrimaryKey(id, key, Some(value))
    }
    
    case Remove(key, id) => {
      kv -= key 
      persistPrimaryKey(id, key, None)
    }
    
    case TimeOut(id) => {
      checkAckStatus(id)
      notAcked.get(id).map(entry => {
        entry._1 ! OperationFailed(id) 
      })
      notAcked -= id
    }
    
    case Persisted(key, id) => {
      notAcked.get(id).map{entry =>
        val (client, persist, replicas) = entry
        notAcked += id -> (client, None, replicas)
        checkAckStatus(id)
      }
    }
    
    case Replicated(key, id) => {
      notAcked.get(id).map{entry =>
        notAcked += id -> (entry._1, entry._2, entry._3 + sender) 
        checkAckStatus(id)
      }
    }
    
    case Replicas(replicas) =>
      var updatedReplicators = Set.empty[ActorRef]
      var updatedSecondaries = Map.empty[ActorRef, ActorRef]
      replicas.map{replica =>
        if(!replica.equals(self)) {
          val replicator = secondary.getOrElse(replica, context.actorOf(Props(classOf[Replicator], replica)))
          if (!secondary.contains(replica)) {
            kv.foreach{entry => replicator ! Replicate(entry._1, Some(entry._2), nextSeq)}
          }
          
          updatedReplicators += replicator
          //Remove the active ones from original set
          replicators -= replicator  
          updatedSecondaries += (replica -> replicator)
        } 
      }
      //Stop left ones in original set
      replicators.foreach(context.stop) 
      replicators = updatedReplicators
      secondary = updatedSecondaries
      notAcked.keySet.foreach(checkAckStatus)
  }

  val replica: Receive = {
    case Get(key, id) => sender ! GetResult(key, kv.get(key), id)
    case Snapshot(key, valueOption, seq) => {
      if (seq == expectedVersion) {
        valueOption match {
          case None => kv -= key
          case Some(value) => kv += key -> value
        }
        persist(seq, key, valueOption) 
      } 
      else if (seq < expectedVersion) sender ! SnapshotAck(key, seq)
    }
    
    case Persisted(key, id) => {
      notAcked.get(id).map(entry => {
        val (requester, persist, _) = entry
        persist.map(cmd => requester ! SnapshotAck(key, cmd.id))
        expectedVersion += 1
      })
      notAcked -= id
    }
    
    case RetryPersist => {
      notAcked.foreach(entry => {
        val (_, (_, command, _)) = entry
        command.map(persistence ! _)
      })
    }
  }
  
  def checkAckStatus(id: Long) = {
    notAcked.get(id).map{entry =>
      val (client, persist, reps) = entry
      persist match {
        case None if replicators.forall(reps.contains(_)) => client ! OperationAck(id); notAcked -= id
        case _ =>
      } 
    }  
  }
  
  def persistPrimaryKey(id: Long, key: String, valueOption: Option[String]) = {
    persist(id, key, valueOption)
    replicators.foreach(_  ! Replicate(key, kv.get(key), id))
    context.system.scheduler.scheduleOnce(1000 millis, context.self, TimeOut(id))
  }
  
  def persist(id: Long, key: String, valueOption: Option[String]) = {
    val command = Persist(key, valueOption, id)
    notAcked += id -> (sender, Some(command), Set.empty[ActorRef]) 
    persistence ! command
  }
 
}
