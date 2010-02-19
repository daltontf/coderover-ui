package tfd.coderover.ui.tasks

import tfd.coderover.ui.GUIEnvironment
import tfd.coderover._

abstract class Task(val title:String, val description:String) {
  def isComplete(environment:GUIEnvironment, state:State):Boolean

  var scenarios:List[Scenario] = _

  override def toString() = title
}

object SimpleTask extends Task("Simple Task", "Simple Task") {
  val startEnvironment = new GUIEnvironment(sizeX = 10, sizeY = 10, targetLocation = Some(8,8))

  scenarios = List(new Scenario("Only Scenario", new State(2,2,1), startEnvironment))

  def isComplete(environment:GUIEnvironment, state:State) = state match {
    case State(8,8,_) => true
    case _ => false
  }

}

object Goto55Task extends Task("Goto 5,5", "Goto 5,5") {

  val startEnvironment = new GUIEnvironment(sizeX = 10, sizeY = 10, targetLocation = Some(5,5))

  scenarios = List(new Scenario("Start at 2,2 - face up", new State(2,2,0), startEnvironment),
                   new Scenario("Start at 2,8 - face left", new State(2,8,1), startEnvironment),
                   new Scenario("Start at 2,8 - face left", new State(8,2,2), startEnvironment),
                   new Scenario("Start at 8,8 - face right", new State(8,8,3), startEnvironment))

  def isComplete(environment:GUIEnvironment, state:State) = state match {
    case State(5,5,_) => true
    case _ => false
  }
}

object MineField extends Task("Goto 5,5 in Minefield", "Goto 5,5") {

  object DetonatedLandMine extends Abend("Detonated a land mine")

  val startEnvironment = new GUIEnvironment(
    sizeX = 10,
    sizeY = 10,
    targetLocation = Some(5,5),
    hiddenEntities = Map("MINE" -> Set((3,3)))) {

      override def postMoveForward(state:State) = {
        if (hiddenEntities.getOrElse("MINE", Set.empty).contains((state.gridX, state.gridY))) {
          Some(DetonatedLandMine)
        } else {
          None
        }
    }
  }

  scenarios = List(new Scenario("Start at 2,2 - face up", new State(2,2,0), startEnvironment))

  def isComplete(environment:GUIEnvironment, state:State) = state match {
    case State(5,5,_) => true
    case _ => false
  }
}

object GotoFlag extends Task("Goto Flag", "Goto Flag") {
  scenarios = List(
    new Scenario("Start at 7,2 - face up", new State(7,2,0), new GUIEnvironment(sizeX = 10, sizeY = 10, visibleEntities = Map("FLAG" -> Set((3,8))))),
    new Scenario("Start at 7,2 - face down", new State(2,7,0), new GUIEnvironment(sizeX = 10, sizeY = 10, visibleEntities = Map("FLAG" -> Set((0,0)))) )
  )

  def isComplete(environment:GUIEnvironment, state:State) = {
    val rc = environment.visibleEntities("FLAG").contains((state.gridX, state.gridY)); 
    println(rc);
    rc }
}

object FollowTheYellowBrickRoad extends Task("Follow the Yellow Brick Road",
  "Navigate to destination touching only yellow ")
{
  object MovedToUnpainted extends Abend("Moved to unpainted square")

  val startEnvironment = new GUIEnvironment(sizeX = 10, sizeY = 10,
    prePainted = Set((2,2), (2,3), (3,3), (3,4), (3,5), (3,6), (2,6), (1,6), (1,7), (1,8),
      (2,8), (3,8), (4,8), (5,8), (5,7), (5,6), (5,5), (5,4), (5,3), (5,2), (5,1), (6,1),
      (7,1), (8,1), (9,1), (9,2), (9,3), (9,4), (8,4), (7,4), (7,5), (7,6), (8,6), (9,6),
      (9,7), (9,8), (8,8)),
    targetLocation = Some(8,8)) {

    override def postMoveForward(state:State) = {
      if (!isPainted(state.gridX, state.gridY)) {
        Some(MovedToUnpainted)
      } else {
        None
      }
    }
   }

  scenarios = List( new Scenario("Scenario 1", new State(2,2,0), startEnvironment))

  def isComplete(environment:GUIEnvironment, state:State) = state match {
    case State(8,8,_) => true
    case _ => false
  }
}

object PaintTheTown extends Task("PaintTheTown", "Paint every accessible square")
{
  scenarios = List(
    new Scenario("Scenario 1", new State(1,1,0), new GUIEnvironment(sizeX = 5, sizeY = 5, obstructed = Set((2,2),(3,1),(1,3)))),
    new Scenario("Scenario 2", new State(3,3,0), new GUIEnvironment(sizeX = 5, sizeY = 5, obstructed = Set((1,2),(2,2),(3,2),(2,1),(2,3))))
  )

  def isComplete(environment:GUIEnvironment, state:State):Boolean = {
    for (x <- 0 to environment.sizeX-1;
         y <- 0 to environment.sizeY-1) {
      if (!environment.obstructed.contains((x,y)) && !environment.isPainted(x,y,state)) {
        return false
      }
    }
    true
  }
}

class TaskManager(tasks:Task*) {
  private val taskArray:Array[Task] = tasks.toArray 
  private var currentTaskIndex = 0

  var allTasksComplete = false

  def nextTask() {
    if (currentTaskIndex < tasks.length - 1) {
      currentTaskIndex += 1
    } else {
      allTasksComplete = true
    }
  }

  def currentTask = tasks(currentTaskIndex)
}

class Scenario(
  val description:String,
  val createStartState:() => State,
  val createStartEnvironment:() => GUIEnvironment,
  val constraints:Constraints
) {
  def this(description:String, state:State, environment:GUIEnvironment) =
    this(description, () => state, () => environment, DefaultConstraints)

  override def toString = description
}
