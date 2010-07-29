package tfd.coderover.ui.tasks

import tfd.coderover.ui.GUIEnvironment
import tfd.coderover._
import xml.{XML, Node, Elem, NodeSeq}
class TaskManager(tasks:Seq[Task]) {
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

object DeserializeTaskManager extends (NodeSeq => TaskManager) {
  override def apply(xml: NodeSeq) = {
    new TaskManager(for (taskXML <- xml \ "task") yield (new XmlTask(taskXML)))
  }
}

abstract class Task() {
  var title:String = ""
  var description:String = ""

  def isComplete(environment:GUIEnvironment, state:State):Boolean

  var scenarios:List[Scenario] = _

  override def toString() = title

  def createState(x:Int, y:Int, direction:Int) = { () => new State(x, y, direction) }
}

class Scenario(
  val description:String,
  val createStartState:() => State,
  val createStartEnvironment:() => GUIEnvironment,
  val constraints:Constraints = DefaultConstraints
) {
  override def toString = description
}

class XmlTask(taskXML:NodeSeq) extends Task() {
  private val evaluator = new Evaluator()
  private var isCompleteExpression:BooleanExpression = _

  private var constraints = DefaultConstraints;

  private def getOrElse[A](first:Option[A],second:Option[A], orElse:A) = first.getOrElse(second.getOrElse(orElse))

  private def parseStartState(xml:NodeSeq) =
    (for (startState <- xml \ "start_state")
      yield (
          Some((startState \ "@x").text.toInt),
          Some((startState \ "@y").text.toInt),
          Some((startState \ "@dir").text.toInt)
      )).headOption.getOrElse(None, None, None)

  private def parseGridSize(xml:NodeSeq) =
      (for (gridSize <- xml \ "grid_size")
        yield (
          Some((gridSize \ "@x").text.toInt),
          Some((gridSize \ "@y").text.toInt)
      )).headOption.getOrElse(None, None)

  private def parseLine(attribute:String, xml:NodeSeq) = {
    val set = new collection.mutable.HashSet[(Int,Int)]()
    for (elem <- xml \ attribute) {
      val x = (elem \ "@x").text.toInt
      val y = (elem \ "@y").text.toInt
      val dx = (elem \ "@dx").text.toInt
      val dy = (elem \ "@dy").text.toInt
      for (i <- 0 until (elem \ "@length").text.toInt) {
        set += ((x + (dx * i), y + (dy * i)))
      }
    }
    set.toSet
  }

  for (titleElem <- taskXML \ "title") {
    title = titleElem.text
  }
  for (descriptionElem <- taskXML \ "description") {
    description = descriptionElem.text
  }
  for (isCompleteElem <- taskXML \ "is_complete") {
    val languageParser = new LanguageParser()
    isCompleteExpression = languageParser.parse(languageParser.booleanExpression, isCompleteElem.text).get
  }
  val (sizeX, sizeY) = parseGridSize(taskXML)
  val (startX, startY, startDir) = parseStartState(taskXML)
  val obstructions = parseLine("obstruction", taskXML)
  val painted = parseLine("paint", taskXML)
  scenarios = (for (scenarioElem <- taskXML \ "scenario")
    yield {
      val (scenarioSizeX, scenarioSizeY) = parseGridSize(scenarioElem)
      val (scenarioStartX, scenarioStartY, scenarioStartDir) = parseStartState(scenarioElem)
      val scenarioObstructions = parseLine("obstruction", scenarioElem)
      val scenarioPainted = parseLine("paint", scenarioElem)

      new Scenario(
        (scenarioElem \ "@title").text,
        createState(
                getOrElse(scenarioStartX, startX, 2),
                getOrElse(scenarioStartY, startY, 2),
                getOrElse(scenarioStartDir, startDir, 0)
        ),
        () => new GUIEnvironment(
          sizeX = getOrElse(scenarioSizeX, sizeX, 10),
          sizeY = getOrElse(scenarioSizeY, sizeY, 10),
          obstructions ++ scenarioObstructions,
          painted ++ scenarioPainted,
          None,
          Map.empty[String, Set[(Int,Int)]],
          Map.empty[String, Set[(Int,Int)]]
        ))
  }).toList

  def isComplete(environment:GUIEnvironment, state:State) = {
    evaluator.evaluateBoolean(isCompleteExpression, Array.empty[Int], new Controller(state, environment, constraints)).value.get
  }  
}

object SimpleXMLTask extends XmlTask(
<task>
  <title>Simple XML Task</title>
  <description>Simple XML Task</description>
  <grid_size x="9" y="9"/>
  <is_complete>((X = 8) AND (Y = 8))</is_complete>
  <start_state x="2" y="2" dir="2"/>
  <scenario title="Only Scenario">
    
  </scenario>
  <scenario title="Only Other Scenario">
    <grid_size x="9" y="9"/>
    <start_state x="3" y="3" dir="2"/>
  </scenario>
</task>
)


object Goto55Task extends Task() {
  title = "Goto 5,5"
  description = "Goto 5,5"

  def createStartEnvironment = { () => new GUIEnvironment(sizeX = 10, sizeY = 10, targetLocation = Some(5,5)) }

  scenarios = List(new Scenario("Start at 2,2 - face up", createState(2,2,0), createStartEnvironment),
                   new Scenario("Start at 2,8 - face left", createState(2,8,1), createStartEnvironment),
                   new Scenario("Start at 2,8 - face left", createState(8,2,2), createStartEnvironment),
                   new Scenario("Start at 8,8 - face right", createState(8,8,3), createStartEnvironment))

  def isComplete(environment:GUIEnvironment, state:State) = state match {
    case State(5,5,_) => true
    case _ => false
  }
}

//object MineField extends Task("Goto 5,5 in Minefield", "Goto 5,5") {
//
//  object DetonatedLandMine extends Abend("Detonated a land mine")
//
//  def createStartEnvironment = { () => new GUIEnvironment(
//    sizeX = 10,
//    sizeY = 10,
//    targetLocation = Some(5,5),
//    hiddenEntities = Map("MINE" -> Set((3,3)))) {
//
//      override def postMoveForward(state:State) = {
//        if (hiddenEntities.getOrElse("MINE", Set.empty).contains((state.gridX, state.gridY))) {
//          Some(DetonatedLandMine)
//        } else {
//          None
//        }
//    }
//  }}
//
//  scenarios = List(new Scenario("Start at 2,2 - face up", createState(2,2,0), createStartEnvironment))
//
//  def isComplete(environment:GUIEnvironment, state:State) = state match {
//    case State(5,5,_) => true
//    case _ => false
//  }
//}

object GotoFlag extends Task() {
  title = "Goto Flag"
  description = "Goto Flag"

  def createStartEnvironmentWithFlagAt(flagX:Int, flagY:Int) = { () =>  new GUIEnvironment(sizeX = 10, sizeY = 10, visibleEntities = Map("FLAG" -> Set((flagX, flagY)))) }

  scenarios = List(
    new Scenario("Start at 7,2 - face up", createState(7,2,0), createStartEnvironmentWithFlagAt(3,8)),
    new Scenario("Start at 7,2 - face down", createState(2,7,0), createStartEnvironmentWithFlagAt(0,0))
  )

  def isComplete(environment:GUIEnvironment, state:State) = environment.visibleEntities("FLAG").contains((state.gridX, state.gridY))
}

//object FollowTheYellowBrickRoad extends Task("Follow the Yellow Brick Road",
//  "Navigate to destination touching only yellow ")
//{
//  object MovedToUnpainted extends Abend("Moved to unpainted square")
//
//  def createStartEnvironment = { () => new GUIEnvironment(sizeX = 10, sizeY = 10,
//    prePainted = Set((2,2), (2,3), (3,3), (3,4), (3,5), (3,6), (2,6), (1,6), (1,7), (1,8),
//      (2,8), (3,8), (4,8), (5,8), (5,7), (5,6), (5,5), (5,4), (5,3), (5,2), (5,1), (6,1),
//      (7,1), (8,1), (9,1), (9,2), (9,3), (9,4), (8,4), (7,4), (7,5), (7,6), (8,6), (9,6),
//      (9,7), (9,8), (8,8)),
//    targetLocation = Some(8,8)) {
//
//    override def postMoveForward(state:State) = {
//      if (!isPainted(state.gridX, state.gridY)) {
//        Some(MovedToUnpainted)
//      } else {
//        None
//      }
//    }
//   }
//  }
//
//  scenarios = List( new Scenario("Scenario 1", createState(2,2,0), createStartEnvironment))
//
//  def isComplete(environment:GUIEnvironment, state:State) = state match {
//    case State(8,8,_) => true
//    case _ => false
//  }
//}

object PaintTheTown extends Task()
{
  title = "PaintTheTown"
  description = "Paint every accessible square"

  def createStartEnvironment(obstructed:Set[(Int,Int)]) = { () => new GUIEnvironment(sizeX = 5, sizeY = 5, obstructed = obstructed) }

  scenarios = List(
    new Scenario("Scenario 1", createState(1,1,0), createStartEnvironment(Set((2,2),(3,1),(1,3)))),
    new Scenario("Scenario 2", createState(3,3,0), createStartEnvironment(Set((1,2),(2,2),(3,2),(2,1),(2,3))))
  )

  def isComplete(environment:GUIEnvironment, state:State):Boolean = {
    for (x <- 0 to environment.sizeX-1;
         y <- 0 to environment.sizeY-1) {
      if (!environment.isObstructed(x,y) && !environment.isPainted(x,y)) {
        return false
      }
    }
    true
  }
}

object FindThePath extends Task()
{
  title = "FindThePath"
  description = "Goto 3,3 with obstructions"

  def createStartEnvironment(obstructed:Set[(Int,Int)]) = { () => new GUIEnvironment(sizeX = 5, sizeY = 5, obstructed = obstructed) }

  scenarios = List(
    new Scenario("Scenario 1", createState(1,2,0), createStartEnvironment(Set((2,2),(3,1),(1,3)))),
    new Scenario("Scenario 2", createState(1,1,0), createStartEnvironment(Set((1,2),(2,2),(3,2),(2,1),(2,3))))
  )

  def isComplete(environment:GUIEnvironment, state:State):Boolean = state match {
    case State(3,3,_) => true
    case _ => false
  }
}
