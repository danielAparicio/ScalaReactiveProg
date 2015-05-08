package suggestions
package gui

import scala.language.reflectiveCalls
import scala.collection.mutable.ListBuffer
import scala.collection.JavaConverters._
import scala.concurrent._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.{ Try, Success, Failure }
import scala.swing.Reactions.Reaction
import scala.swing.event.Event
import rx.lang.scala.Observable
import rx.lang.scala.Subscription




/** Basic facilities for dealing with Swing-like components.
*
* Instead of committing to a particular widget implementation
* functionality has been factored out here to deal only with
* abstract types like `ValueChanged` or `TextField`.
* Extractors for abstract events like `ValueChanged` have also
* been factored out into corresponding abstract `val`s.
*/
trait SwingApi {

  type ValueChanged <: Event

  val ValueChanged: {
    //IMPORTANT: unapply / extractors, to get content from an object in the way that we want
    def unapply(x: Event): Option[TextField]
  }

  //This means that Event is an upper bound of ButtonClicked
  //IMP: we can define types with this
  type ButtonClicked <: Event

  val ButtonClicked: {
    def unapply(x: Event): Option[Button]
  }

  //IMP Magic anonymous structural type
  /*
  TextField is a subclass of the (anonymous) structural type that provides the subscribe and unsubscribe methods shown.
  So B is 'still there' (the entire block between {...} ), but un-named, and constrained to implement certain methods. 
  In other words, TextField is defined as anything that has subscribe and unsubscribe methods.
   */
  type TextField <: {
    def text: String
    def subscribe(r: Reaction): Unit
    def unsubscribe(r: Reaction): Unit
  }

  type Button <: {
    def subscribe(r: Reaction): Unit
    def unsubscribe(r: Reaction): Unit
  }
  //implicit class for the TextField Class , NOT for the companion object!!!
  implicit class TextFieldOps(field: TextField) {

    /** Returns a stream of text field values entered in the given text field.
      *
      * @param field the text field
      * @return an observable with a stream of text field updates
      */
    //the Observable factory method is without type
    def textValues: Observable[String] =  Observable(
    											observer=>{
    											  //we create a reaction for a particular event , the event is ValueChanged so in
    											  //case that we get that event we get the text from and we pushed it to the observer
    											  val reaction: Reaction = {
    											      //we get the full text
													  case ValueChanged(textF) => observer.onNext(textF.text)
													  //we need to add this in order to avoid the error in the app execution we need to control all the 
													  //scenarios for a reaction
													  case _ =>
  }
    											  //we subscribe the field to the reaction that we have just created
    											  //this field is the object that is calling the method IMP as this implicit clarr
    											  //is extending the methods of the TextField class
    											  field.subscribe(reaction)
    											  //we return a subscription that contains the body needed to unsuscribe
    											  Subscription{ field.unsubscribe(reaction)}
    											})
 
  }//IMP: Here the creation of the observable is not as the one in the lectures as in the lectures
  //the source is another observable , here the source is the TextField

  implicit class ButtonOps(button: Button) {

    /** Returns a stream of button clicks.
     *
     * @param field the button
     * @return an observable with a stream of buttons that have been clicked
     */
    //same skeleton as the Text Value but with different events
    def clicks: Observable[Button] = Observable(
    											observer=>{
     											  val reaction: Reaction = {
     											      //here instead of the text of the textfield we pass the button that has been clicked
													  case ButtonClicked(click) => observer.onNext(click)
													  //we need to add this in order to avoid the error in the app execution we need to control all the 
													  //scenarios for a reaction
													  case _ =>
											      }
    		    								  button.subscribe(reaction)
    											  Subscription{ button.unsubscribe(reaction)}
    											})

  }

}
