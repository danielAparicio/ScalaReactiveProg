package suggestions



import language.postfixOps
import scala.concurrent._
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.{Try, Success, Failure}
import rx.lang.scala._
import org.scalatest._
import gui._

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner



@RunWith(classOf[JUnitRunner]) //The JUnitRunner of ScalaTest
class WikipediaApiTest extends FunSuite {

  object mockApi extends WikipediaApi {
    def wikipediaSuggestion(term: String) = Future {
      if (term.head.isLetter) {
        for (suffix <- List(" (Computer Scientist)", " (Footballer)")) yield term + suffix
      } else {
        List(term)
      }
    }
    def wikipediaPage(term: String) = Future {
      "Title: " + term
    }
  }

  import mockApi._

  test("WikipediaApi should make the stream valid using sanitized") {
    val notvalid = Observable.just("erik", "erik meijer", "martin")
    val valid = notvalid.sanitized

    var count = 0
    var completed = false

    val sub = valid.subscribe(
      term => {
        assert(term.forall(_ != ' '))
        count += 1
      },
      t => assert(false, s"stream error $t"),
      () => completed = true
    )
    assert(completed && count == 3, "completed: " + completed + ", event count: " + count)
  }
  
  test("recovered") {
    // E.g. `1, 2, 3, !Exception!` should become `Success(1), Success(2), Success(3), Failure(Exception), !TerminateStream!`
    val e: Exception = new Exception
    //IMP: this is a hint of the assignment of how to finish a observable with an exception CONCATENATION OF OBSSERVABLES
    val obs: Observable[Int] = Observable.from(List(1,2,3)) ++ Observable.error(e)
    val result = obs.recovered
    //IMP: we need to use the dangerous method toBlockingObservable in order to parse the observable to a list
    assert(result.toBlocking.toList === List(Success(1),Success(2),Success(3),Failure(e)))
  }
  
  test("timeout") {
    //sometimes with 0.25 interval and 1 second timeout is failing and only counting 3
    val requests = Observable.interval(0.24.second).timedOut(1)
    assert(requests.toBlocking.toList.length === 4)   
  }
  
  test("recovered with element after error") {
    // E.g. `1, 2, 3, !Exception!` should become `Success(1), Success(2), Success(3), Failure(Exception), !TerminateStream!`
    val e: Exception = new Exception
    //IMP: this is a hint of the assignment of how to finish a observable with an exception CONCATENATION OF OBSSERVABLES
    val obs: Observable[Int] = Observable.from(List(1,2,3)) ++ Observable.error(e) ++ Observable.from(List(4))
    val result = obs.recovered
    //IMP: After the exception we shouldn't receive anything the source Observable should stop!!!
    assert(result.toBlocking.toList === List(Success(1),Success(2),Success(3),Failure(e)))
  }
  
  test("WikipediaApi should correctly use concatRecovered") {
    val requests = Observable.just(1, 2, 3)
    val remoteComputation = (n: Int) => Observable.just(0 to n : _*)
    val responses = requests concatRecovered remoteComputation
    val sum = responses.foldLeft(0) { (acc, tn) =>
      tn match {
        case Success(n) => acc + n
        case Failure(t) => throw t
      }
    }
    
    var total = -1
    val sub = sum.subscribe {
      s => total = s
    }
    assert(total == (1 + 1 + 2 + 1 + 2 + 3), s"Sum: $total")
  }

  test("WikipediaApi should correctly use concatRecovered Exception") {
    val e: Exception = new Exception
    val requests = Observable.from(List(1, 2, 3)) ++ Observable.error(e) 
    val remoteComputation = (n: Int) => Observable.from(0 to n)
    val responses = requests concatRecovered remoteComputation
    val sum = responses.foldLeft(0) { (acc, tn) =>
      tn match {
        case Success(n) => acc + n
        case Failure(t) => throw t
      }
    }
    
    var total = -1
    val sub = sum.subscribe {
      s =>  total = s 
    }
    assert(total == (1 + 1 + 2 + 1 + 2 + 3), s"Sum: $total")
  }  
}
