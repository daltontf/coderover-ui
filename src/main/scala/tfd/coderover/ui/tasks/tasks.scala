package tfd.coderover.ui.tasks

import tfd.coderover._
import ui.{GUIViewController, GUIEnvironment}
import xml.{XML, Node, Elem, NodeSeq}
import collection.immutable.{HashSet, HashMap}

class TaskManager(tasks:Seq[Task]) {
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
    new TaskManager(for (taskXML <- xml \ "task") yield (CreateTaskFromXML(taskXML)))
  }
}

class Task (
  val title:String,
  val description:String,
  val scenarios:List[Scenario])
{
  override def toString() = title
}

abstract trait Scenario {
  val description:String

  def createController():GUIViewController

  override def toString = description

  def isComplete(environment:GUIEnvironment, state:State):Boolean
}

object CreateTaskFromXML extends (NodeSeq => Task) {
  lazy private val evaluator = new Evaluator()
  lazy private val languageParser = new LanguageParser()

  private val constraints = DefaultConstraints;

  private def extractTarget(xy:(Option[Int], Option[Int])) = {
    for (xp <- xy._1; yp <- xy._2) yield((xp,yp))
  }

  private def extractParsedBooleanExpression(element:String, xml:NodeSeq) =
    for (
      isCompleteElem <- (xml \ element).headOption
    ) yield (
      languageParser.parse(languageParser.booleanExpression, isCompleteElem.text).get
    )

  private def getOrElse[A](first:Option[A], second:Option[A], orElse:A) = first.getOrElse(second.getOrElse(orElse))

  private def parseStartState(xml:NodeSeq) =
    (for (startState <- xml \ "startState")
    yield (
        Some((startState \ "@x").text.toInt),
        Some((startState \ "@y").text.toInt),
        Some((startState \ "@dir").text.toInt)
        )).headOption.getOrElse(None, None, None)

  private def parseCoordinate(element:String, xml:NodeSeq) =
    (for (elem <- xml \ element)
    yield (
        Some((elem \ "@x").text.toInt),
        Some((elem \ "@y").text.toInt)
        ))

  private def parseLine(element:String, xml:NodeSeq) = {
    val set = new collection.mutable.HashSet[(Int,Int)]()
    for (elem <- xml \ element) {
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

  private def createState(x:Int, y:Int, direction:Int) = new State(x, y, direction)


  override def apply(taskXML:NodeSeq) = {
    val isCompleteExpression = extractParsedBooleanExpression("isComplete", taskXML)
    val postMoveForwardExpression = extractParsedBooleanExpression("postMoveForward", taskXML)
    val (sizeX, sizeY) = parseCoordinate("gridSize", taskXML).headOption.getOrElse(None, None)
    val (startX, startY, startDir) = parseStartState(taskXML)
    val obstructions = parseLine("obstruction", taskXML)
    val painted = parseLine("paint", taskXML)
    val target = extractTarget(parseCoordinate("target", taskXML).headOption.getOrElse(None, None))
    val flags = parseCoordinate("flag", taskXML)

    new Task(
      (for (titleElem <- taskXML \ "title") yield (titleElem.text)).headOption.getOrElse("No Title"),
      (for (descriptionElem <- taskXML \ "description") yield (descriptionElem.mkString)).headOption.getOrElse(""),
      (for (scenarioElem <- taskXML \ "scenario")  yield {
        val (scenarioSizeX, scenarioSizeY) = parseCoordinate("gridSize", scenarioElem).headOption.getOrElse(None, None)
        val (scenarioStartX, scenarioStartY, scenarioStartDir) = parseStartState(scenarioElem)
        val scenarioObstructions = parseLine("obstruction", scenarioElem)
        val scenarioPainted = parseLine("paint", scenarioElem)
        val scenarioTarget = extractTarget(parseCoordinate("target", scenarioElem).headOption.getOrElse(None, None))
        val scenarioFlags = parseCoordinate("flag", scenarioElem)
        val hidden = (for (hidden <- scenarioElem \ "hidden")
          yield  (
            ((hidden \ "@name").text,
              (for (location <- hidden \ "location") yield (((location \ "@x").text.toInt, (location \ "@y").text.toInt))).toSet)
          )
        ).toMap
    val scenarioIsCompleteExpression = extractParsedBooleanExpression("isComplete", scenarioElem)
    val scenarioPostMoveForwardExpression = extractParsedBooleanExpression("postMoveForward", scenarioElem)
        new Scenario {
          override val description = (scenarioElem \ "@title").text

          override def createController() =
            new GUIViewController(45,
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
                hidden
              ), if (scenarioPostMoveForwardExpression.isDefined) scenarioPostMoveForwardExpression else postMoveForwardExpression,
              DefaultConstraints
            )

          override def isComplete(environment:GUIEnvironment, state:State) = {
            val expression = if (scenarioIsCompleteExpression.isDefined) scenarioIsCompleteExpression else isCompleteExpression
            if (expression.isDefined) {
              evaluator.evaluateBoolean(expression.get, Array.empty[Int], new Controller(state, environment, constraints)).value.get
            } else {
              false
            }
          }
        }
      }).toList
    )
  }
}