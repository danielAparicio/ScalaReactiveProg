package simulations

import math.random
import scala.util.Random


class EpidemySimulator extends Simulator {

  //IMP Random : random from math class generates a random value 0.0-1.0 we transform it to get 
  //a random number of the range that we want, generates a rando number below the int i
  def randomBelow(i: Int) = (random * i).toInt
  //another way to generate a random : Random.nextInt(A.size) this is with random class
  
  def getCoordenate(position : Int) = position match {
    case -1 => 7
    case 8 => 0
    case _ => position
  }  
  def getAvailableRooms (neighbourRooms: List[(Int,Int)]): List[(Int,Int)] = {
    //we keep in the list the coordenates that has people infected
    neighbourRooms.filter(c => persons.count(p => p.row == c._1 && p.col ==c._2 && p.sick)==0)    
  }

  //scoped (package) protected object (can be applied also to classes and members) that can be accessed by all the package simulations
  protected[simulations] object SimConfig {
    val population: Int = 300
    val roomRows: Int = 8
    val roomColumns: Int = 8
    val maxTimeToMove: Int = 5
    val infectPercentage: Int = 40
    val diePercentage: Int = 25
    val iniInfectedPercentage: Int = 1
  }

  // we import the object and we can use the methods directly 
  import SimConfig._

  //as we import the object "import SimConfig._" we can use directly the vals of the object
  def isInfected(): Boolean = if ((random * 100).toInt <= infectPercentage ) true else false 
  //here we can omit the return type and its inferred (also we could have done that in the upper function)
  //we generate a random value between 1 and 100 and in it falls into the range of die percentage 0-25		
  
  def isDeath() = if ((random * 100).toInt <= diePercentage ) true else false
  
  val persons: List[Person] = {
    var count = 1
    val initialInfectedNumber = population * iniInfectedPercentage / 100
    (for {      
      i <- 1 to population
      person = new Person(i)     
      //We we could put a condition in order to add or don't add the person , but that is not the logic that we need here  for example  "if(person.dead == true)"
      //But we cannot have here the same condition that we have in the yield clause
     } yield {
      //The condition for the yield , depending on a condition we modify the person or not, but in both cases we return a person for yielding  
      //for the initially sick persons we need to add the "afterDelay(6)(person.checkSick)" otherwise it will be always infected and never sick as per the implementation of person
      if( count <= initialInfectedNumber ) { count = count + 1 ; person.infected = true; afterDelay(6)(person.checkSick); person  }
      else person 
    }).toList
    
  }
   //we have to initialize it here if you do it within the persons construction....you get a Null
   //dont tell me why I know move is duplicated but the intax was not working only with an if, I think that is
   //because with foreach is a function x => f(X)
   persons.foreach(p => p.move(true))
  
  class Person (val id: Int) {
    var infected = false
    var sick = false
    var immune = false
    var dead = false

    // demonstrates random number generation
    var row: Int = randomBelow(roomRows)
    var col: Int = randomBelow(roomColumns)
    
    def move(initial : Boolean) {     
      //dead people doesn't move     
      if(!dead){  
        //We can use a val inside the method as in every execution we will have a new value
        val neighRooms = List((row,getCoordenate(col-1)),(getCoordenate(row-1),col),(row,getCoordenate(col+1)),(getCoordenate(row+1),col))
     	val availableRooms = getAvailableRooms(neighRooms)
      	//we only move if there is any room with non visibly sick people     	
    	if(availableRooms.length > 0){	
    	  val selectedRoom = availableRooms(randomBelow(availableRooms.length)) 
    	  //updating the new position
    	  row = selectedRoom._1
    	  col = selectedRoom._2
             	
    	}
        //we need to schedule a move anyway and check if there is someone infected in that room, check the infection is
        //quite important as otherwise we sill end up without with an steady state of people not infected
    	if (!initial && persons.filter(p => p.row == row && p.col == col && p.infected ).length > 0) { checkInfected }  
    	// first time only movement initial, here we doesn't check for infection
        afterDelay(randomBelow(maxTimeToMove)+1)(move(false)) 
      } 
    }
      
    def checkInfected = {      
      if(!infected && !immune){
	      if(isInfected) { infected = true ; afterDelay(6)(checkSick) }	      
       }    
    }
    
    //After being infected always get sick
    def checkSick={       
    	sick = true
    	afterDelay(8)(checkDead)   
    }
    
    def checkDead={
      //this if is just becaus one test is overriding the natural process of infection-sick-dead and
      //setting a guy directly from sick to dead without playing with the probability
       if(!dead){
    	   if (isDeath) dead = true 
    	   else afterDelay(2)(setImmune)
       }	
	}
	 
    //IMPORTAT, initially without the brackets and obviously after delay is out of the def and in executed
    def setImmune = { sick = false ; immune = true ; afterDelay(2)(healthy) }
	
    //IMPORTANT same as above but setting members instead of executing a method
	def healthy = { infected= false ; immune = false; }
  }
}
