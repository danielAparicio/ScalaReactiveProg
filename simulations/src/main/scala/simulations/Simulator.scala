package simulations

class Simulator {
  //a type with no params and returning nothing
  type Action = () => Unit

  protected type Agenda = List[WorkItem]

  case class WorkItem(time: Int, action: Action)

  //protected but scoped to simulations pkg level to have visibility from EmpidemySuite
  protected[simulations] var agenda: Agenda = List()
  protected var currentTime = 0
  									  //parameter passed by name that returns nothing
  									 //otherwise we would evaluate it and is not necessary at all
  protected def afterDelay(delay: Int)(action: => Unit) {
    //we save it as () => action, where action is a block of code
    val item = WorkItem(currentTime + delay, () => action)
    def insert(ag: Agenda): Agenda =
      if (ag.isEmpty || item.time < ag.head.time) item :: ag
      else ag.head :: insert(ag.tail)
    agenda = insert(agenda)
  }

  protected[simulations] def next {
    agenda match {
      case List() => {}
      case WorkItem(time, action) :: rest =>
        agenda = rest
        currentTime = time
        //here we execute the action that as we can see in afterdelay is a function "() =>  block of code"
        action()
    }
  }

  def run {
    println("*** New propagation ***")
    while (!agenda.isEmpty) { next }
  }
}
