import scala.language.postfixOps
import scala.io.StdIn
import scala.util._
import scala.util.control.NonFatal
import scala.concurrent._
import scala.concurrent.duration._
import ExecutionContext.Implicits.global
import scala.async.Async.{async, await}
import java.util.NoSuchElementException
import scala.util.control.Exception



/** Contains basic data types, data structures and `Future` extensions.
 */
package object nodescala {

  /** Adds extensions methods to the `Future` companion object.
   */
  //IMP: Implicit Class
  //The implicit modifier on the class declaration above means that the compiler will generate an implicit conversion 
  //from the Future companion object to the FutureCompanionOps object
  //The extends AnyVal part is just an optimization telling the compiler to avoid instantiating 
  //the FutureCompanionOps object where possible and call its methods directly.
  
  //Summary: The bottom line is that whenever you want to add missing methods to an already 
  //existing class implementation, you should use this pattern.
  
  //This implicit conversion will be called every time you call a non-existing method on the Future companion object
  implicit class FutureCompanionOps[T](val f: Future.type) extends AnyVal {

    /** Returns a future that is always completed with `value`.
     */
    def always[T](value: T): Future[T] = {      
    	val p =Promise[T]();
    	//the promise is expecting a try so we build one with the value and is always
    	//returning the value
    	p complete(scala.util.Try(value));      
      p.future
    }

    /** Returns a future that is never completed.
     *
     *  This future may be useful when testing if timeout logic works correctly.
     */
    def never[T]: Future[T] =  Promise[T]().future
    //returning the future within the promise that is non completed
    //if we do this with a eternal loop is a massive screw as the thread will be busy forever

    /** Given a list of futures `fs`, returns the future holding the list of values of all the futures from `fs`.
     *  The returned future is completed only once all of the futures in `fs` have been completed.
     *  The values in the list are in the same order as corresponding futures `fs`.
     *  If any of the futures `fs` fails, the resulting future also fails.
     */
  //def all[T](fs: List[Future[T]]): Future[List[T]] = allasync(fs);
    def all[T](fs: List[Future[T]]): Future[List[T]] = {
      val p = Promise[List[T]]
      p.success(Nil)
      fs.foldRight(p.future) {
        (f, acc) => for{ x <- f; xs <- acc } yield x :: xs
      }
    }

    /** Given a list of futures `fs`, returns the future holding the value of the future from `fs` that completed first.
     *  If the first completing future in `fs` fails, then the result is failed as well.
     *
     *  E.g.:
     *
     *      Future.any(List(Future { 1 }, Future { 2 }, Future { throw new Exception }))
     *
     *  may return a `Future` succeeded with `1`, `2` or failed with an `Exception`.
     */
    def any[T](fs: List[Future[T]]): Future[T] = {     
      val p = Promise[T]();
      //we do a tryComplete over all the futures on complete the first one that is completed will be the 
      //value of the future within the promise, the onComplete call will not have effect (will not throw) when
      //called over the rest futures
      fs foreach (f => f onComplete { 
        p.tryComplete(_)
      });
      //we are completing the future of the promise with whatever value we have on _ that is a Try[T]
      // this is also valid :
      //        fs foreach(f => f onComplete {x => p.tryComplete(x) });
      //the upper one is more elegant
       
      p.future     
    }

    /** Returns a future with a unit value that is completed after time `t`.
     */
    def delay(t: Duration): Future[Unit] = {      
    	Future { Thread.sleep(t.toMillis) }
    	//IMP: this should be a better way to do that but I was unable to do it with Await.result as the future was never completed	     
    }    
    	

    /** Completes this future with user input.
     */
    def userInput(message: String): Future[String] = Future {
      blocking {
        StdIn.readLine(message)
      }
    }

    /** Creates a cancellable context for an execution and runs it.
     */
    def run()(f: CancellationToken => Future[Unit]): Subscription = {
      
      val cts = CancellationTokenSource()
      f(cts.cancellationToken) 
      cts     
    }

  }
  //This ERROR is nomal, its compiling a working perfectly
  def allasync[T](fs: List[Future[T]]): Future[List[T]] = async {
      //sorry do this with promises is a mess , an absolute mess
      //create a new list for the results
      var futureList:List[T]  = Nil;
      //var with the initial list to loop thorugh it
      var _fs = fs
      //we loop through the list getting the future result
      while(!_fs.isEmpty){
       //we add the result into the list 
       futureList.+:(await(_fs.head))       
       //the loop
       _fs= _fs.tail 
      } 
      //IMP we return the list that is converted to a Future[List[T]] by async
      futureList
    }

  /** Adds extension methods to future objects.
   */
  //IMPORTANT: this is the way to add methods to a instance of a class , check the difference FutureCompanionOps[T](val f: Future.type) extends AnyVal
  implicit class FutureOps[T](val f: Future[T]) extends AnyVal {

    /** Returns the result of this future if it is completed now.
     *  Otherwise, throws a `NoSuchElementException`.
     *
     *  Note: This method does not wait for the result.
     *  It is thus non-blocking.
     *  However, it is also non-deterministic -- it may throw or return a value
     *  depending on the current state of the `Future`.
     */
    def now: T = {
	    //here we don't need to use await to wait for 0 secs as we are doing nothing with that , we can directly check the value of 
	    //the future in the moment of the method execution
	    f.value match {
	      //None is future not completed
			  case None => throw new NoSuchElementException
        //get method is autmatically throwing an exception if it's a failure so we don't need to specify the failure case 
        case Some(x) => x.get
	    }	
    }

    /** Continues the computation of this future by taking the current future
     *  and mapping it into another future.
     *
     *  The function `cont` is called only after the current future completes.
     *  The resulting future contains a value returned by `cont`.
     */
    def continueWith[S](cont: Future[T] => S): Future[S] = {
      //IMP: we don't need another future here only process the result of the original one scala.util.Try(cont(f))     
      val p = Promise[S]();      
      f.onComplete { x => p.complete(scala.util.Try(cont(f)))}       
      p.future;    
    }

    /** Continues the computation of this future by taking the result
     *  of the current future and mapping it into another future.
     *
     *  The function `cont` is called only after the current future completes.
     *  The resulting future contains a value returned by `cont`.
     */
    def continue[S](cont: Try[T] => S): Future[S] = { 
      //IMP: we don't need another future here only process the result of the original one
      val p = Promise[S]();
      //IMP: onComplete is giving us back the Try[T] within x, then we can go more deep doing a pat matching of that try
      //as the standard try with a case Success(y) or a Failure(t) and we will get the T=y or the exception t
      f.onComplete { x => p.complete(scala.util.Try(cont(x)))
//        more complex and verbose        
//        case Success(x) => p.complete(scala.util.Try(cont(scala.util.Try(x))))   
//        case Failure(t) => p.failure(t)        
      } 
      //IMP: typical structure with future ,here we are returning the reference to the future within the promise we will check outside when it's completed as we return a future
      p.future;      
    }    
  }

  /** Subscription objects are used to be able to unsubscribe
   *  from some event source.
   */
  trait Subscription {
    def unsubscribe(): Unit
  }

  object Subscription {
    /** Given two subscriptions `s1` and `s2` returns a new composite subscription
     *  such that when the new composite subscription cancels both `s1` and `s2`
     *  when `unsubscribe` is called.
     */
    def apply(s1: Subscription, s2: Subscription) = new Subscription {
      def unsubscribe() {
        s1.unsubscribe()
        s2.unsubscribe()
      }
    }
  }

  /** Used to check if cancellation was requested.
   */
  trait CancellationToken {
    def isCancelled: Boolean
    def nonCancelled = !isCancelled
  }

  /** The `CancellationTokenSource` is a special kind of `Subscription` that
   *  returns a `cancellationToken` which is cancelled by calling `unsubscribe`.
   *
   *  After calling `unsubscribe` once, the associated `cancellationToken` will
   *  forever remain cancelled -- its `isCancelled` will return `false.
   */
  trait CancellationTokenSource extends Subscription {
    def cancellationToken: CancellationToken
  }

  /** Creates cancellation token sources.
   */
  //This is the object  to create objects of the type CancellationTokenSource that is a TRAIT, so
  //we can have companion objects with TRAITS but we should implement all the methods of the TRAIT
  object CancellationTokenSource {
    /** Creates a new `CancellationTokenSource`.
     */
    def apply() = new CancellationTokenSource {
    //we use a promise as is thread safe
      val p = Promise[Unit]()
      //Here we override a def with a val , as val is more restrictive we can do it (remember the scala book)
      val cancellationToken = new CancellationToken {
      //checks the value of the future of the promise
        def isCancelled = p.future.value != None
      }
      def unsubscribe() {
        //this will complete the promise (the future of the promise) with a unit so the future will not have None as value
        p.trySuccess(())
      }
    }
  }
}

