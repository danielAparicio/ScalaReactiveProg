package suggestions
package observablex

import scala.concurrent.{Future, ExecutionContext}
import scala.util._
import scala.util.Success
import scala.util.Failure
import java.lang.Throwable
import rx.lang.scala.Observable
import rx.lang.scala.Scheduler
import rx.lang.scala.subjects.ReplaySubject
import scala.concurrent.Await
import scala.language.postfixOps
import scala.concurrent.duration._
import rx.lang.scala.subjects.PublishSubject
import java.util.concurrent.TimeoutException


object ObservableEx {

  /** Returns an observable stream of values produced by the given future.
   * If the future fails, the observable will fail as well.
   *
   * @param f future whose values end up in the resulting observable
   * @return an observable completed after producing the value of the future, or with an exception
   */
  //This apply will be called many times (so will create a lot of observables) as its inside a map so 
  //should be completed properly otherwise everything will get stuck , think about this  
  def apply[T](f: Future[T])(implicit execContext: ExecutionContext): Observable[T] = {
    //IMP: subject is BOTH the Observable and the Observer
    val subject = ReplaySubject[T]()
    //alternative:  val subject :ReplaySubject[T]= ReplaySubject()
    //I guess that we could use a PublishSubject but the instantiation with the factory method apply is not working
    
    f.onComplete {
        case Success(x) => subject.onNext(x); subject.onCompleted() 
        									//IMP :without the subject onCompleted the observable get stuck too
        									//should be completed either with an onComlete or onError
        case Failure(t) => subject.onError(t)
  }
    //we need to wait for the future to finish but not more than 2 seconds
    //IMP: If we do not handle the timeout exception that Await Throws everything will CRASH timeoutException not handled
    try{      
      Await.ready(f, 2 seconds)
    }catch {
	  case t:TimeoutException => subject.onError(t)
	}          
    //return a subject is ok to satisfy the Observable[T] as subject EXTENDS OBERVABLE
    subject
    
  } 
    
    

}