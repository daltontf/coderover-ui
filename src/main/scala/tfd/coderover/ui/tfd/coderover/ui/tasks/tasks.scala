package tfd.coderover.ui.tasks

import tfd.coderover._
import ui.{GUIViewController, GUIEnvironment}
import xml.{XML, Node, Elem, NodeSeq}
import collection.immutable.{HashSet, HashMap}

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

  def createState(x:Int, y:Int, direction:Int) = new State(x, y, direction)
}

class Scenario(
  val description:String,
  val createController:() => GUIViewController
) {
  override def toString = description
}

class XmlTask(taskXML:NodeSeq) extends Task() {
  private val evaluator = new Evaluator()
  private var isCompleteExpression:BooleanExpression = _
  private var postMoveForwardExpression:Option[BooleanExpression] = None

  private var constraints = DefaultConstraints;

  private def getOrElse[A](first:Option[A],second:Option[A], orElse:A) = first.getOrElse(second.getOrElse(orElse))

  private def parseStartState(xml:NodeSeq) =
    (for (startState <- xml \ "start_state")
      yield (
          Some((startState \ "@x").text.toInt),
          Some((startState \ "@y").text.toInt),
          Some((startState \ "@dir").text.toInt)
      )).headOption.getOrElse(None, None, None)

  private def parseCoordinate(attribute:String, xml:NodeSeq) =
      (for (elem <- xml \ attribute)
        yield (
          Some((elem \ "@x").text.toInt),
          Some((elem \ "@y").text.toInt)
      ))

  private def parseLine(attribute:String, xml:NodeSeq) = {
    val set = new collection.mutable.HashSet[(Int,Int)]()
    for (elem <- xml \ attribute) {
      val x = (elem \ "@x").text.toInt
      val y = (elem \ "@y").text.toInt
      val dx = (elem \ "@dx").text
      val dy = (elem \ "@dy").text
      if (dx.length > 0) {
        for (i <- Range(0, dx.toInt, if (dx.toInt < 0) -1 else 1)) {
          set += (((x + i) , y))
        }
      }
      if (dy.length > 0) {
        for (i <- Range(0, dy.toInt, if (dy.toInt < 0) -1 else 1)) {
          set += ((x , (y + i)))
        }
      }
    }
    set.toSet
  }

  private def extractTarget(xy:(Option[Int], Option[Int])) = {
    for (xp <- xy._1; yp <- xy._2) yield((xp,yp))
  }

  for (titleElem <- taskXML \ "title") {
    title = titleElem.text
  }
  for (descriptionElem <- taskXML \ "description") {
    description = descriptionElem.text
  }
  lazy val languageParser = new LanguageParser()
  for (isCompleteElem <- taskXML \ "is_complete") {
    isCompleteExpression = languageParser.parse(languageParser.booleanExpression, isCompleteElem.text).get
  }
  for (postMoveForward <- taskXML \ "post_move_forward") {
    postMoveForwardExpression = Some(languageParser.parse(languageParser.booleanExpression, postMoveForward.text).get)
  }
  val (sizeX, sizeY) = parseCoordinate("grid_size", taskXML).headOption.getOrElse(None, None)
  val (startX, startY, startDir) = parseStartState(taskXML)
  val obstructions = parseLine("obstruction", taskXML)
  val painted = parseLine("paint", taskXML)
  val target = extractTarget(parseCoordinate("target", taskXML).headOption.getOrElse(None, None))
  val flags = parseCoordinate("flag", taskXML)

  scenarios = (for (scenarioElem <- taskXML \ "scenario")
    yield {
      val (scenarioSizeX, scenarioSizeY) = parseCoordinate("grid_size", scenarioElem).headOption.getOrElse(None, None)
      val (scenarioStartX, scenarioStartY, scenarioStartDir) = parseStartState(scenarioElem)
      val scenarioObstructions = parseLine("obstruction", scenarioElem)
      val scenarioPainted = parseLine("paint", scenarioElem)
      val scenarioTarget = extractTarget(parseCoordinate("target", scenarioElem).headOption.getOrElse(None, None))
      val scenarioFlags = parseCoordinate("flag", scenarioElem)

      new Scenario(
        (scenarioElem \ "@title").text,
          () => new GUIViewController(50,
            createState(
                getOrElse(scenarioStartX, startX, 2),
                getOrElse(scenarioStartY, startY, 2),
                getOrElse(scenarioStartDir, startDir, 0)
            ),new GUIEnvironment(
                getOrElse(scenarioSizeX, sizeX, 10),
                getOrElse(scenarioSizeY, sizeY, 10),
                obstructions ++ scenarioObstructions,
                painted ++ scenarioPainted,
                if (scenarioTarget.isDefined) scenarioTarget else target,
                Map("FLAG" -> (for (coord <-  flags ++ scenarioFlags;
                     x <- coord._1;
                     y <- coord._2) yield (x,y)).toSet),
                Map.empty[String, Set[(Int,Int)]]
            ), DefaultConstraints
            ) {

            override def postMoveForward():Option[Abend] =
              if (postMoveForwardExpression.isDefined &&
                  evaluator.evaluateBoolean(postMoveForwardExpression.get, Array.empty[Int], new Controller(this.state, this.environment, constraints)).value.get) {
                  Some(new Abend("PostMoveForward") {})
                } else {
                  None
                }
            })
  }).toList

  override def isComplete(environment:GUIEnvironment, state:State) = {
    evaluator.evaluateBoolean(isCompleteExpression, Array.empty[Int], new Controller(state, environment, constraints)).value.get
  }
}

