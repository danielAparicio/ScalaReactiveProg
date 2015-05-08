package suggestions
package gui


import scala.language.postfixOps
import scala.concurrent.duration._
import scala.collection.mutable.ListBuffer
import scala.collection.JavaConverters._
import scala.concurrent._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.swing._
import scala.util.{ Try, Success, Failure }
import scala.swing.event._
import swing.Swing._
import javax.swing.UIManager
import Orientation._
import rx.subscriptions.CompositeSubscription
import rx.lang.scala.Observable
import rx.lang.scala.Subscription
import observablex._
import search._




object WikipediaSuggest extends SimpleSwingApplication with ConcreteSwingApi with ConcreteWikipediaApi {

  {
    try {
      UIManager.setLookAndFeel(UIManager.getSystemLookAndFeelClassName())
    } catch {
      case t: Throwable =>
    }
  }

  def top = new MainFrame {

    /* gui setup */

    title = "Query Wikipedia"
    minimumSize = new Dimension(900, 600)

    val button = new Button("Get") {
      icon = new javax.swing.ImageIcon(javax.imageio.ImageIO.read(this.getClass.getResourceAsStream("/suggestions/wiki-icon.png")))
    }
    val searchTermField = new TextField
    val suggestionList = new ListView(ListBuffer[String]())
    val status = new Label(" ")
    val editorpane = new EditorPane {
      import javax.swing.border._
      border = new EtchedBorder(EtchedBorder.LOWERED)
      editable = false
      peer.setContentType("text/html")
    }

    contents = new BoxPanel(orientation = Vertical) {
      border = EmptyBorder(top = 5, left = 5, bottom = 5, right = 5)
      contents += new BoxPanel(orientation = Horizontal) {
        contents += new BoxPanel(orientation = Vertical) {
          maximumSize = new Dimension(240, 900)
          border = EmptyBorder(top = 10, left = 10, bottom = 10, right = 10)
          contents += new BoxPanel(orientation = Horizontal) {
            maximumSize = new Dimension(640, 30)
            border = EmptyBorder(top = 5, left = 0, bottom = 5, right = 0)
            contents += searchTermField
          }
          contents += new ScrollPane(suggestionList)
          contents += new BorderPanel {
            maximumSize = new Dimension(640, 30)
            add(button, BorderPanel.Position.Center)
          }
        }
        contents += new ScrollPane(editorpane)
      }
      contents += status
    }

    val eventScheduler = SchedulerEx.SwingEventThreadScheduler

    /**
     * Observables
     * You may find the following methods useful when manipulating GUI elements:
     *  `myListView.listData = aList` : sets the content of `myListView` to `aList`
     *  `myTextField.text = "react"` : sets the content of `myTextField` to "react"
     *  `myListView.selection.items` returns a list of selected items from `myListView`
     *  `myEditorPane.text = "act"` : sets the content of `myEditorPane` to "act"
     */

    //In the text values we create and observable of string based on  subscribing the Textfied to a reaction that is giving
    //us back the text field content everytime we click
    val searchTerms: Observable[String] = searchTermField.textValues

    //the observable searchTerms is giving us every time the full text of the textField (not only the key that we have pressed)
    val suggestions: Observable[Try[List[String]]] = 
    									//we use ObservableEx to create an observable passing as param a funtion
      									//that returns a future
      	searchTerms.concatRecovered (x => ObservableEx(wikipediaSuggestion(x)))

											//This observable should be run in an specific thread
    val suggestionSubscription: Subscription =  suggestions.observeOn(eventScheduler) subscribe {
      x => x match {
        	case Success(x) => suggestionList.listData = x
        	case Failure(t) => status.text = t.getMessage()
      	   }
      //we recover all the errors with a Failure(t) so no throwable error caseis possible anyway lets control it..
      t:Throwable => status.text = t.getMessage() + ": Wrong not controlled"
      //() =>   
    }

    val selections: Observable[String] =
      Observable(observer =>
        {
          button.clicks.subscribe {
        	  					//based on the assignment we could have several items selected in the UI but for this exercise
        	  					//the length will always be 0 or 1.
            x => if(suggestionList.selection.items.length > 0) { observer.onNext(suggestionList.selection.items(0)) }
          }
        })

	 //IMP: here we pass a function that returns a Future that is what ObservaleEx needs
	 //the other option of passing a function as param is use it inside the method (also the type required is different X => U), 
	 //but in this case we only use the value that the wikipediaPage(x) returns
    val pages: Observable[Try[String]] = selections.concatRecovered (x => ObservableEx(wikipediaPage(x)))
    															//getting the page content based in the selection

    val pageSubscription: Subscription = pages.observeOn(eventScheduler) subscribe {
      x => x match {
        	case Success(x) => editorpane.text = x
        	case Failure(t) => status.text = t.getMessage() ; editorpane.text = t.getMessage()
    }
  }

}

}

trait ConcreteWikipediaApi extends WikipediaApi {
  def wikipediaSuggestion(term: String) = Search.wikipediaSuggestion(term)
  def wikipediaPage(term: String) = Search.wikipediaPage(term)
}


trait ConcreteSwingApi extends SwingApi {
  type ValueChanged = scala.swing.event.ValueChanged
  object ValueChanged {
    def unapply(x: Event) = x match {
      case vc: ValueChanged => Some(vc.source.asInstanceOf[TextField])
      case _ => None
    }
  }
  type ButtonClicked = scala.swing.event.ButtonClicked
  object ButtonClicked {
    def unapply(x: Event) = x match {
      case bc: ButtonClicked => Some(bc.source.asInstanceOf[Button])
      case _ => None
    }
  }
  type TextField = scala.swing.TextField
  type Button = scala.swing.Button
}
