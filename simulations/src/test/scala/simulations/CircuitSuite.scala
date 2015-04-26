package simulations

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class CircuitSuite extends CircuitSimulator with FunSuite {
  val InverterDelay = 1
  val AndGateDelay = 3
  val OrGateDelay = 5
  
  test("andGate example") {
    val in1, in2, out = new Wire
    andGate(in1, in2, out)
    in1.setSignal(false)
    in2.setSignal(false)
    run
    
    assert(out.getSignal === false, "and 1")

    in1.setSignal(true)
    run
    
    assert(out.getSignal === false, "and 2")

    in2.setSignal(true)
    run
    
    assert(out.getSignal === true, "and 3")
  }
  
  test("Gate Or and Or2 example"){
    
    val in1, in2,in3, in4, out,out2 = new Wire
    orGate(in1, in2, out)
    orGate2(in3, in3, out2)
   
    in1.setSignal(false)
    in2.setSignal(false)
    
    in3.setSignal(false)
    in4.setSignal(false)
    run
         
    assert(out.getSignal === false, "or 1")
    assert(out2.getSignal === false, "or2 1")

    in1.setSignal(true)
    in3.setSignal(true)
    run
    
    assert(out.getSignal === true, "or 2")
    assert(out2.getSignal === true, "or2 2")

    in2.setSignal(true)
    in4.setSignal(true)
    run
    
    assert(out.getSignal === true, "or 3")
    assert(out2.getSignal === true, "or2 3")
        
  }
  
  test("demux"){
    
    val in,cont1, cont0, out3, out2, out1, out0 = new Wire
    demux(in,List(cont1,cont0),List(out3,out2,out1,out0))
    
    //we also could have checked that length of the list of control and out wires
    //but was not part of the scope of the assignment 
    in.setSignal(true)
    cont1.setSignal(true)
    cont0.setSignal(false)   
    out3.setSignal(false)
    out2.setSignal(false)
    out1.setSignal(false)
    out0.setSignal(false)
    
    run
    
    assert(out3.getSignal === false, "demux level 3")
    assert(out2.getSignal === true, "demux level 2")
    assert(out1.getSignal === false, "demux level 1")
    assert(out0.getSignal === false, "demux level 0")
    
  }

}
