package tfd.coderover.ui

abstract class Task(val title:String, val description:String, val scenarios:Scenario*) {
  val taskState = new TaskState
  
  def stateIsComplete(environment:Environment, state:State):Boolean
  
  def createNewEnvironment():GUIEnvironment
  
  override def toString() = title
}

class TaskState {
  val taskIsComplete = false
  val code = ""
}

object SimpleTask extends Task("Simple Task", 
                               "Simple Task", 
                               new StartStateScenario(new State(2,2,1), "Only Scenario")) {
  
  def stateIsComplete(environment:Environment, state:State) = state match {
    case State(8,8,_) => true
    case _ => false
  }
  
  def createNewEnvironment() = new GUIEnvironment(10, 10, Some(8,8))
}

object Goto55Task extends Task("Goto 5,5",
                               "Goto 5,5",
                               new StartStateScenario(new State(2,2,0), "Start at 2,2 - face up"),
                               new StartStateScenario(new State(2,8,1), "Start at 2,8 - face left"),
                               new StartStateScenario(new State(8,2,2), "Start at 8,2 - face down"),
                               new StartStateScenario(new State(8,8,3), "Start at 8,8 - face right")
) {
  def stateIsComplete(environment:Environment, state:State) = state match {
    case State(5,5,_) => true
    case _ => false
  }
  
  def createNewEnvironment() = new GUIEnvironment(10, 10, Some(5,5))
 }
  
object TaskManager {
  private val task:Array[Task] = Array(SimpleTask, Goto55Task)
  private var currentTaskIndex = 0
  
  var allTasksComplete = false
  
  def nextTask() { 
	if (currentTaskIndex < task.length - 1) {
	  currentTaskIndex += 1
	} else {
	  allTasksComplete = true
	}
  }
      
  def currentTask = task(currentTaskIndex)
}

abstract class Scenario(val description:String) {
  def createStartState():State
  
  override def toString = description
}

class StartStateScenario(state:State, description:String) extends Scenario(description) {
	def createStartState() = State(state.gridX, state.gridY, state.directionIndex)
}