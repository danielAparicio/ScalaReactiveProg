package simulations

import common._
import scala.annotation.tailrec

//why not Wire an inner class of Circuit ????
class Wire {
  private var sigVal = false
  // IMP : The class wire doesn't extends Simulator thus doesn't have visibility of Action type (is protected)
  //with the Simulator#Action we can use it
  //Protected only visible by subclasses 
  private var actions: List[Simulator#Action] = List()

  def getSignal: Boolean = sigVal
  
  def setSignal(s: Boolean) {
    if (s != sigVal) {
      sigVal = s
      actions.foreach(action => action())
    }
  }

  def addAction(a: Simulator#Action) {
    actions = a :: actions
    a()
  }
}

abstract class CircuitSimulator extends Simulator {

  val InverterDelay: Int
  val AndGateDelay: Int
  val OrGateDelay: Int

  def probe(name: String, wire: Wire) {
    //IMP also { } could be used instead of ( ) to wrap addAction parameters
    wire addAction ( 
      () => afterDelay(0) {
        println(
          "  " + currentTime + ": " + name + " -> " +  wire.getSignal)
      }
    )
  }

  def inverter(input: Wire, output: Wire) {
    def invertAction() {
      val inputSig = input.getSignal
      afterDelay(InverterDelay) { output.setSignal(!inputSig) }
    }
    input addAction invertAction
  }
  
  // IMPORTAT ()=>Unit and {} equals unit
  def andGate(a1: Wire, a2: Wire, output: Wire) {
    //this def matches Action type is a parameterless function that returns nothing (()=>Unit)
    //then inside Wire we do action() ,in other words ,we execute the def
    def andAction() {
      val a1Sig = a1.getSignal
      val a2Sig = a2.getSignal
      //the content inside brackets is the part of the action parameter in the afterdelay method
      //when we execute the steps of the agenda we add this block that is a unit as result of 
      //parameterless function thus we have in the end an Action ()=>Unit
      afterDelay(AndGateDelay) { output.setSignal(a1Sig & a2Sig) }
      							//IMP *we also could use ( )instead of { }*/
    }
    a1 addAction andAction
    a2 addAction andAction
  }

 
  def orGate(a1: Wire, a2: Wire, output: Wire) {
    def orAction(){
      val a1Sig = a1.getSignal
      val a2Sig = a2.getSignal
      afterDelay(OrGateDelay) { output.setSignal(a1Sig || a2Sig) }
    }
    a1 addAction orAction
    a2 addAction orAction
  }
  
  def orGate2(a1: Wire, a2: Wire, output: Wire) {
    //Implement orGate in terms of inverter and andGate
    val aux1,aux2,aux3 = new Wire
    inverter(a1,aux1)
    inverter(a2,aux2)
    andGate(aux1,aux2,aux3)
    inverter(aux3,output)
  }

  def demux(in: Wire, c: List[Wire], out: List[Wire]) {
	val initWire = new Wire
	initWire.setSignal(true)
	val controlLenght = c.length
	
	//we have the control length and in in the def demux scope and we are not using it for recursion params so 
	//we don't have to pass them as params of the Aux function
	//IMP: example with two recursive calls in a row as its recursive we need to specify the return type
    def demuxAux(prevWire:Wire,c: List[Wire], out: List[Wire]):Unit={	
	  
	  //if the control levels are empty we have to do a and gate with the input an the wire with the signal accumulated
	  if (c.isEmpty) andGate(prevWire, in, out.head) 
	  else{
	    
		  //first level of control (otherwise we will include an extra andGate)
	      if (c.length == controlLenght){
	        
	        //the first half of the outputs of this lever is not inverted, the second half is inverted
	        val AuxInverter = new Wire
	        inverter(c.head, AuxInverter)
	                                //method to split a list
	        demuxAux(c.head,c.tail, out.splitAt(out.length / (2))._1)
	        //the second part of the output list has the signal inverted
	        demuxAux(AuxInverter,c.tail, out.splitAt(out.length / (2))._2)
	        
	      }else{
	        
	        //the first half of the outputs of this lever is not inverted, the second half is inverted 
	        //plus and and operation with the previous value
	        val AuxInverter, andGateResult, andGateResultInverted  = new Wire
	        inverter(c.head, AuxInverter)
	        andGate(prevWire, c.head, andGateResult)
	        andGate(prevWire, AuxInverter, andGateResultInverted)
	        					           //method to split a list
	        demuxAux(andGateResult,c.tail, out.splitAt(out.length / (2))._1)
	        demuxAux(andGateResultInverted,c.tail, out.splitAt(out.length / (2))._2)
	        
	      }	    
	  } 	  
	}
        
    demuxAux(initWire,c,out)
        
//    This implementation is useless as Fucking odersky wants it with the and, or and inverter gates
//    with recursion we always need a return type
//    @tailrec
//    def controlWeight(c: List[Wire],weightAcc:Int):Int={
//      if(c.isEmpty) weightAcc
//      else {
//        //we calculate the weight of that control position with pow
//    	  if(c.head.getSignal == true) controlWeight(c.tail,weightAcc.+(math.pow(2,c.length-1).intValue())) 
//    	  else controlWeight(c.tail,weightAcc)
//      }
//      
//    }
//     
//    out(controlWeight(c,0)).setSignal(in.getSignal)
//    out foreach (elem =>println(elem.getSignal))
  }

}

object Circuit extends CircuitSimulator {
  val InverterDelay = 1
  val AndGateDelay = 3
  val OrGateDelay = 5

  def andGateExample {
    val in1, in2, out = new Wire
    andGate(in1, in2, out)
    probe("in1", in1)
    probe("in2", in2)
    probe("out", out)
    in1.setSignal(false)
    in2.setSignal(false)
    run

    in1.setSignal(true)
    run

    in2.setSignal(true)
    run
  }

  //
  // to complete with orGateExample and demuxExample...
  //
}

object CircuitMain extends App {
  // You can write tests either here, or better in the test class CircuitSuite.
  //finally done in the CircuitSuite
  Circuit.andGateExample
}
