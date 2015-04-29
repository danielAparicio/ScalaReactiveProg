package nodescala

import com.sun.net.httpserver._
import scala.concurrent._
import scala.concurrent.duration.Duration
import ExecutionContext.Implicits.global
import scala.async.Async.{async, await}
import scala.collection._
import scala.collection.JavaConversions._
import java.util.concurrent.{Executor, ThreadPoolExecutor, TimeUnit, LinkedBlockingQueue}
import com.sun.net.httpserver.{HttpExchange, HttpHandler, HttpServer}
import java.net.InetSocketAddress
import scala.util.Try
import scala.concurrent.duration.Duration.Infinite

/** Contains utilities common to the NodeScala© framework.
 */
trait NodeScala {
  import NodeScala._

  def port: Int

  def createListener(relativePath: String): Listener

  /** Uses the response object to respond to the write the response back.
   *  The response should be written back in parts, and the method should
   *  occasionally check that server was not stopped, otherwise a very long
   *  response may take very long to finish.
   *
   *  @param exchange     the exchange used to write the response back
   *  @param token        the cancellation token 
   *  @param body         the response to write back
   */
  private def respond(exchange: Exchange, token: CancellationToken, response: Response): Unit = {
    //TODO check id there is any way to break a for loop in scala
    //response.foreach(s => exchange.write(s); token.nonCancelled)
    
    //IMP: we can place the asyc call (Future{}) here or when the start method is calling respond method Future {respond}
    Future {
      while(response.hasNext && token.nonCancelled){     
        exchange.write(response.next)      
      }
      exchange.close
    }  
   }

  /** A server:
   *  1) creates and starts an http listener
   *  2) creates a cancellation token (hint: use one of the `Future` companion methods)
   *  3) as long as the token is not cancelled and there is a request from the http listener
   *     asynchronously process that request using the `respond` method
   *
   *  @param relativePath   a relative path on which to start listening on
   *  @param handler        a function mapping a request to a response
   *  @return               a subscription that can stop the server and all its asynchronous operations *entirely*.
   */
  //IMP: good examples of futures within a while loop and while loop within async
  def start(relativePath: String)(handler: Request => Response): Subscription = {
    val listener = createListener(relativePath);
    //we need to start the listener....otherwise we will be listening to nothing
    //and when the token is cancelled we should unsubscribe
    val listenerSubscription = listener.start()

    /*IMPORTNATN about exceptions in async{} , if we have an exception awaiting for a future within an async{} block
    the Future returned by async{} block is a failed future with that exception */
    
    Future.run() { ct =>
      async {
        while (ct.nonCancelled) {
          //to be cooler we can use val (a,b) for tuples , and then use a and b in the rest of the code
          val request = await { listener.nextRequest }
          //println("puta vida :"+ request)
          //IMP : This call is going to be async within this block we don't need to wrap it in a future and then do an await...
          respond(request._2, ct, handler(request._1))
        }
        listenerSubscription.unsubscribe()
      }
    } 
  }  
}
  //      Version without async    
  //      Future { 
  //        //we can include futures in a while loop
  //        while(ct.nonCancelled){ 
  //          //we block here as it's specified in the exercise (we can unsusbscribe) 
  //          //Await.result bit is a very dangerous operation even more with the Duration.Inf
  //          val request: (Request, Exchange)= Await.result(listener.nextRequest, Duration.Inf)
  //          respond(request._2, ct, handler(request._1))
  //          //val request: Future[(Request, Exchange)] = listener.nextRequest
  //          //request.map{ r => respond(r._2, ct, handler(r._1)) }           
  //        }
  //        listenerSubscription.unsubscribe()
  //      }

object NodeScala {

  /** A request is a multimap of headers, where each header is a key-value pair of strings.
   */
  type Request = Map[String, List[String]]

  /** A response consists of a potentially long string (e.g. a data file).
   *  To be able to process this string in parts, the response is encoded
   *  as an iterator over a subsequences of the response string.
   */
  type Response = Iterator[String]

  /** Used to write the response to the request.
   */
  trait Exchange {
    /** Writes to the output stream of the exchange.
     */
    def write(s: String): Unit

    /** Communicates that the response has ended and that there
     *  will be no further writes.
     */
    def close(): Unit

    def request: Request

  }

  object Exchange {
    def apply(exchange: HttpExchange) = new Exchange {
      val os = exchange.getResponseBody()
      exchange.sendResponseHeaders(200, 0L)

      def write(s: String) = os.write(s.getBytes)

      def close() = os.close()

      def request: Request = {
        val headers = for ((k, vs) <- exchange.getRequestHeaders) yield (k, vs.toList)
        immutable.Map() ++ headers
      }
    }
  }

  trait Listener {
    def port: Int

    def relativePath: String

    def start(): Subscription

    def createContext(handler: Exchange => Unit): Unit
    //The handler function will get the Exchange and do the process with that Exchange

    def removeContext(): Unit

    /** This Method:
     *  1) constructs an uncompleted promise
     *  2) installs an asynchronous `HttpHandler` to the `server`
     *     that deregisters itself
     *     and then completes the promise with a request when `handle` method is invoked
     *  3) returns the future with the request
     *
     *  @return                the promise holding the pair of a request and an exchange object
     */
    def nextRequest(): Future[(Request, Exchange)] = {
      val p = Promise[(Request, Exchange)]()

      createContext(xchg => {
        val req = xchg.request
        removeContext()
        p.success((req, xchg))
      })

      p.future
    }
  }

  object Listener {
    class Default(val port: Int, val relativePath: String) extends Listener {
      private val s = HttpServer.create(new InetSocketAddress(port), 0)
      private val executor = new ThreadPoolExecutor(1, 1, 0L, TimeUnit.MILLISECONDS, new LinkedBlockingQueue)
      s.setExecutor(executor)

      def start() = {
        s.start()
        new Subscription {
          def unsubscribe() = {
            s.stop(0)
            executor.shutdown()
          }
        }
      }

      def createContext(handler: Exchange => Unit) = s.createContext(relativePath, new HttpHandler {
        def handle(httpxchg: HttpExchange) = handler(Exchange(httpxchg))
      })

      def removeContext() = s.removeContext(relativePath)
    }
  }

  /** The standard server implementation.
   */
  class Default(val port: Int) extends NodeScala {
    def createListener(relativePath: String) = new Listener.Default(port, relativePath)
  }

}
