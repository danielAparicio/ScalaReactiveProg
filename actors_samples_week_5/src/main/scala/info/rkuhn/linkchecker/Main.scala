package info.rkuhn.linkchecker

import akka.actor.Actor
import akka.actor.Props
import scala.concurrent.duration._
import akka.actor.ReceiveTimeout

class Main extends Actor {
  
  import Receptionist._

  val receptionist = context.actorOf(Props[Receptionist], "receptionist")
  
  receptionist ! Get("http://www.google.com")
  receptionist ! Get("http://www.google.com/1")
  receptionist ! Get("http://www.google.com/2")
  receptionist ! Get("http://www.google.com/3")
  receptionist ! Get("http://www.google.com/4")
  
  //a timeout of 10 seconds between messages
  context.setReceiveTimeout(10.seconds)
  
  //SHEDULEONCE : this timeout will be fired after 10 seconds of actor's start , 
  // and will send the message Timeout to the ActorRef (self) , so we have to receive it and handle it
  //not depending on the time between messages
  //context.system.scheduler.scheduleOnce(10.seconds,self,Timeout) { code to execute after timeout }
  
  def receive = {
    case Result(url, set) =>
      println(set.toVector.sorted.mkString(s"Results for '$url':\n", "\n", "\n"))
    case Failed(url) =>
      println(s"Failed to fetch '$url'\n")
    case ReceiveTimeout =>
      //this will stop the children too, and waterfall
      context.stop(self)
  }
  
  override def postStop(): Unit = {
    AsyncWebClient.shutdown()
  }
  
}