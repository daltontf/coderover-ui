package tfd.coderover.ui.tasks

import tfd.coderover.{Abend, Environment, State}
import tfd.coderover.ui.GUIEnvironment

abstract class Task(val title:String, val description:String, val scenarios:Scenario*) {
  def stateIsComplete(environment:Environment, state:State):Boolean
  
  def createNewEnvironment():GUIEnvironment
  
  override def toString() = title
}

object SimpleTask extends Task("Simple Task", 
                               "Simple Task", 
                               new StartStateScenario(new State(2,2,1), "Only Scenario")) {
  
  def stateIsComplete(environment:Environment, state:State) = state match {
    case State(8,8,_) => true
    case _ => false
  }
  
  def createNewEnvironment() = new GUIEnvironment(10, 10, Set((1,1)), Some(8,8))
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

object FollowTheYellowBrickRoad extends Task("Follow the Yellow Brick Road",
                                             "Navigate to destination touching only yellow ",
                                              new StartStateScenario(new State(2,2,0), "Scenario 1"))
 {
  object MovedToUnpainted extends Abend("Moved to unpainted square")

  def stateIsComplete(environment:Environment, state:State) = state match {
    case State(8,8,_) => true
    case _ => false
  }

  def createNewEnvironment() = new GUIEnvironment(10, 10, Some(8,8)) {
      override def  postMoveForward(state:State) {
        if (!isPainted(state.gridX, state.gridY)) {
          state.fail(MovedToUnpainted)
        }
      }

      override def reset() {
        super.reset()
        val painted = Array(
          (2,2), (2,3), (3,3), (3,4), (3,5), (3,6), (2,6), (1,6), (1,7), (1,8), (2,8), (3,8),
          (4,8), (5,8), (5,7), (5,6), (5,5), (5,4), (5,3), (5,2), (5,1), (6,1), (7,1), (8,1),
          (9,1), (9,2), (9,3), (9,4), (8,4), (7,4), (7,5), (7,6), (8,6), (9,6), (9,7), (9,8),
          (8,8))
        painted.map { t => paint(t._1, t._2) }
      }
  }
}
  
object TaskManager {
  private val task:Array[Task] = Array(SimpleTask, Goto55Task, FollowTheYellowBrickRoad)
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