package tfd.coderover.ui

import _root_.tfd.coderover._
import java.awt.{Color, Dimension, Graphics}
import java.awt.image.BufferedImage
import java.awt.geom.AffineTransform

import edu.umd.cs.piccolo.PCanvas
import java.awt.event.MouseEvent;
import edu.umd.cs.piccolo.nodes.PImage;

class GUIViewController(
        squareSize:Int,
        val startState:State,
        val environment:GUIEnvironment,
        val postMoveForwardExpression:Option[BooleanExpression],
        constraints:Constraints) extends Controller(startState.copy(), environment, constraints)
{
  state = startState

  lazy private val evaluator = new Evaluator()

  var printDelegate: Option[String => Unit] = _

	private var transform:AffineTransform = _
      
	private val canvas = new PCanvas() {

    override def getToolTipText(event: MouseEvent) = {
      val x = event.getX() / squareSize
      val y = event.getY() / squareSize
      if (x < environment.sizeX && y  < environment.sizeY) {
        "X = " + x + ", Y = " + y
      } else {
        null
      }
    }
  }
  canvas.setToolTipText("") // Bug in Piccolo requires this for the override to work?

  private val background = new PImage()

  private val rover = new PImage(makePaintedImage(squareSize, squareSize, { g:Graphics =>
        	g.setColor(Color.RED)
        	val oneHalf = squareSize/2;
        	val oneFifth = squareSize/5;
        	val fourFifths = oneFifth * 4
          	g.drawLine(oneHalf, oneFifth, oneFifth, fourFifths)
            g.drawLine(oneFifth, fourFifths, fourFifths, fourFifths)
            g.drawLine(fourFifths, fourFifths, oneHalf, oneFifth)
  }))
   

  private def executeAnimation(duration:Int) {
		rover.animateToTransform(transform, duration )
		Thread.sleep(duration)
	}
  
  private def makePaintedImage(width:Int, height:Int, painter:Graphics => Unit) = {
		val image = new BufferedImage(width, height, BufferedImage.TYPE_4BYTE_ABGR)
    	painter(image.getGraphics)
    	image
  }

  private def buildBackground() = makePaintedImage(environment.sizeX * squareSize + 1, environment.sizeY * squareSize + 1, { g:Graphics =>
    g.setColor(Color.BLACK)

    for (i <- 0 to environment.sizeX) {
      g.drawLine(i * squareSize, 0, i * squareSize, environment.sizeY * squareSize - 1)
    }
    for (i <- 0 to environment.sizeY) {
      g.drawLine(0, i * squareSize, environment.sizeX * squareSize - 1, i * squareSize)
    }
  })

  private def renderPaint(color:Color, gridX:Int, gridY:Int) {
    val g = background.getImage.getGraphics
		g.setColor(color)
		g.fillRect(gridX * squareSize + 1, gridY * squareSize + 1, squareSize-1, squareSize-1)
  }

  override def paint() {
    environment.paint(gridX, gridY)
    renderPaint(Color.YELLOW, gridX, gridY)
  }

  def syncToStartState() {
    state = startState
		transform = new AffineTransform()
		transform.translate(state.gridX * squareSize, state.gridY * squareSize)
		transform.rotate(state.directionIndex * (java.lang.Math.PI/2), squareSize/2, squareSize/2)
		rover.setTransform(transform)
	}

  def syncEnvironment() {
    background.setImage(buildBackground())
    for (x <- 0 to environment.sizeX-1; y <- 0 to environment.sizeY-1) {
      if (environment.isPainted(x,y)) {
        renderPaint(Color.YELLOW, x, y)
      }
      if (environment.isObstructed(x,y)) {
        renderPaint(Color.BLACK, x, y)
      }
    }
  }

  override def print(value:String) =  printDelegate.map{ f => f(value) }

  override def executeMoveForward() = {
	  super.executeMoveForward()
	  transform.translate(0, -squareSize)
    executeAnimation(500)
	}

  override def postMoveForward():Option[Abend] =
    if (postMoveForwardExpression.isDefined &&
      evaluator.evaluateBoolean(postMoveForwardExpression.get, Array.empty[Int], this).value.get) {
      Some(new Abend("PostMoveForward") {})
    } else {
      None
    }

	override def turnRight() = {
	  super.turnRight()
	  transform.rotate(java.lang.Math.PI/2, squareSize/2, squareSize/2)
	  executeAnimation(1000)
    }
  
	override def turnLeft() = {
	  super.turnLeft()
	  transform.rotate(-java.lang.Math.PI/2, squareSize/2, squareSize/2)
	  executeAnimation(1000)
	}

  def reset() {
    environment.reset()
    resetState()
  }

  def getView:java.awt.Component = canvas

  def repaint() {
    canvas.repaint()
  }

  def currentState() = state.copy()

  private def placePImage(pImage:PImage, coordinates:(Int,Int)) {
      val (x,y) = coordinates
      transform = new AffineTransform()
      transform.translate(x * squareSize, y * squareSize)
      pImage.setTransform(transform)
      background.addChild(pImage)
  }

  canvas.setPreferredSize(new Dimension(environment.sizeX * squareSize, environment.sizeY * squareSize)) 
  canvas.getLayer.addChild(background)
  canvas.setPanEventHandler(null);
  syncEnvironment()

  if (environment.targetLocation != None) {
      placePImage(new PImage(makePaintedImage(squareSize, squareSize, { g:Graphics =>
    		g.setColor(Color.BLUE)
        g.drawArc(1, 1, squareSize - 2, squareSize - 2, 0, 360);
      })), environment.targetLocation.get)
  }

  for (elements <- environment.visibleEntities.get("FLAG");
       (coordinates, index) <- elements.zipWithIndex) {
    placePImage(new PImage(makePaintedImage(squareSize, squareSize, { g:Graphics =>
      g.setColor(Color.BLUE)
      val oneFourth = squareSize / 4;
      g.drawLine(oneFourth, 5, oneFourth, squareSize - 5);
      g.drawLine(oneFourth, 5, squareSize-oneFourth, oneFourth);
      g.drawLine(squareSize-oneFourth, oneFourth, oneFourth, oneFourth * 2);
      g.drawString("" + (index + 1), squareSize / 2, squareSize - oneFourth)
    })), coordinates)
  }

  background.addChild(rover)
}
