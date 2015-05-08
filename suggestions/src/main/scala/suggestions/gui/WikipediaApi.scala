package suggestions
package gui

import scala.language.postfixOps
import scala.collection.mutable.ListBuffer
import scala.collection.JavaConverters._
import scala.concurrent._
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.{ Try, Success, Failure }
import rx.subscriptions.CompositeSubscription
import rx.lang.scala.Observable
import observablex._
import search._
import rx.lang.scala.Notification
import scala.util.Success



trait WikipediaApi {

  /** Returns a `Future` with a list of possible completions for a search `term`.
   */
  def wikipediaSuggestion(term: String): Future[List[String]]

  /** Returns a `Future` with the contents of the Wikipedia page for the given search `term`.
   */
  def wikipediaPage(term: String): Future[String]

  /** Returns an `Observable` with a list of possible completions for a search `term`.
   */
  def wikiSuggestResponseStream(term: String): Observable[List[String]] = ObservableEx(wikipediaSuggestion(term)).timedOut(1L)

  /** Returns an `Observable` with the contents of the Wikipedia page for the given search `term`.
   */
  def wikiPageResponseStream(term: String): Observable[String] = ObservableEx(wikipediaPage(term)).timedOut(1L)

  implicit class StringObservableOps(obs: Observable[String]) {

    /** Given a stream of search terms, returns a stream of search terms with spaces replaced by underscores.
     *
     * E.g. `"erik", "erik meijer", "martin` should become `"erik", "erik_meijer", "martin"`
     */
    //we apply the map combinator over the observable that is calling the sanitized method
    def sanitized: Observable[String] = obs.map(x=> x.replace(" ", "_"))

  }

  implicit class ObservableOps[T](obs: Observable[T]) {

    /** Given an observable that can possibly be completed with an error, returns a new observable
     * with the same values wrapped into `Success` and the potential error wrapped into `Failure`.
     *
     * E.g. `1, 2, 3, !Exception!` should become `Success(1), Success(2), Success(3), Failure(Exception), !TerminateStream!`
     */
    //finally didin't know how to fit the OnCompleted case....thanks for the HINT!! jajajaja
    //we don manage on complete and the observable never ends
   /* def recovered: Observable[Try[T]] = obs.materialize.map(x=>x match {
    															//Also could be Try(elem) but Success is a subtype of Try
      															case Notification.OnNext(elem) => Success(elem)
      															//we need to do this because in the lectures Onxxx it suppossed
      															//to be a case class of Notification but IS NOT
      															case Notification.OnError(e) => Failure(e) 
      															//we do not handle OnComplete case
      															case _:Notification.OnCompleted[_] => "pepe"
    														})*/
    
    
    //for every new item (remember that we get them one by one and we don't know when we will get the next)
    //we receive an item and we mapped it to Success(v) and if there is an error we wrap in into a failure 
    def recovered: Observable[Try[T]] = obs.map(v => Success(v)).onErrorReturn(t => Failure(t))
    								//we couldn't do it the other way around as onError will return a Failure(t) and map cannot handle it
    								//as map could receive a T or a Failure the common Type (of T and Try(throwable)) is Any
    
    
    //alternative with apply version
  /*def recovered: Observable[Try[T]] = Observable { observer =>
      obs.subscribe(
        t => observer.onNext(Success(t)),
        { error: Throwable =>
          //on error we do an onNext wrapping the error and the an onCompleted
          observer.onNext(Failure(error))
          observer.onCompleted //On Error the observable stream has to be terminated
        },
        () => observer.onCompleted
      )
    }* */

    /** Emits the events from the `obs` observable, until `totalSec` seconds have elapsed.
     *
     * After `totalSec` seconds, if `obs` is not yet completed, the result observable becomes completed.
     *
     * Note: uses the existing combinators on observables.
     */
    //WTF is not available the method take (duration) would be straightforward and in the API it is
    //as we don't have that method we have to do this ugly thing
    def timedOut(totalSec: Long): Observable[T] = obs.takeUntil(Observable.interval(totalSec seconds).take(1))


    /** Given a stream of events `obs` and a method `requestMethod` to map a request `T` into
     * a stream of responses `S`, returns a stream of all the responses wrapped into a `Try`.
     * The elements of the response stream should reflect the order of their corresponding events in `obs`.
     *
     * E.g. given a request stream:
     *
     * 1, 2, 3, 4, 5
     *
     * And a request method:
     *
     * num => if (num != 4) Observable.just(num) else Observable.error(new Exception)
     *
     * We should, for example, get:
     *
     * Success(1), Success(2), Success(3), Failure(new Exception), Success(5)
     *
     *
     * Similarly:
     *
     * Observable(1, 2, 3).concatRecovered(num => Observable(num, num, num))
     *
     * should return:
     *
     * Observable(Success(1), Succeess(1), Succeess(1), Succeess(2), Succeess(2), Succeess(2), Succeess(3), Succeess(3), Succeess(3))
     */
    //def concatRecovered[S](requestMethod: T => Observable[S]): Observable[Try[S]] = obs.flatMap(requestMethod(_)).recovered
    //IMP: The problem with flatmap is that is non deterministic , we may will not get the right order
    def concatRecovered[S](requestMethod: T => Observable[S]): Observable[Try[S]] = 
      obs.map(requestMethod(_).recovered).concat   //TODO !!!!IMPORTANTTT how should we control the error of the first observable??
                             //recovered is only a transformation of the current observable result
  }

}

