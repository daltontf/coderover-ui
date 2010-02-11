package tfd.coderover.ui

import java.awt.{Color, Dimension, Graphics}
import java.awt.image.BufferedImage
import java.awt.geom.AffineTransform

import edu.umd.cs.piccolo.PCanvas;
import edu.umd.cs.piccolo.nodes.PImage;

import _root_.tfd.coderover.{Controller, DefaultConstraints, State}

class GUIViewController(var squareSize:Int, var environment:GUIEnvironment) extends Controller(new State(0,0,0), environment, DefaultConstraints) {
	private var transform:AffineTransform = _
      
	private[ui] val canvas = new PCanvas()

  private val background = new PImage()

  val robot = new PImage(makePaintedImage(squareSize,squareSize, { g:Graphics =>
        	g.setColor(Color.RED)
        	val oneHalf = squareSize/2;
        	val oneFifth = squareSize/5;
        	val fourFifths = oneFifth * 4
          	g.drawLine(oneHalf, oneFifth, oneFifth, fourFifths)
            g.drawLine(oneFifth, fourFifths, fourFifths, fourFifths)
            g.drawLine(fourFifths, fourFifths, oneHalf, oneFifth)
  }))
   
  private val layer = canvas.getLayer()
  
  private def executeAnimation(duration:Int) {
		robot.animateToTransform(transform, duration ) 
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
       		g.drawLine(i * squareSize, 0, i * squareSize, environment.sizeX * squareSize - 1)
       	}
    	for (i <- 0 to environment.sizeY) {
        	g.drawLine(0, i * squareSize, environment.sizeY * squareSize - 1, i * squareSize)
        }
    	if (environment.targetLocation != None) {
    		val coordinates = environment.targetLocation.get
    		g.drawArc(coordinates._1 * squareSize + 1, 
    			      coordinates._2 * squareSize + 1,
    			      squareSize - 2,
    			      squareSize - 2,
    			      0,
    			      360);
      
    	}
      environment.visibleEntities.getOrElse("FLAG", Set.empty).map {
        case (x,y) => g.drawLine(x * squareSize, y * squareSize, (x+1) * squareSize , (y+1) * squareSize)
      }
  	})

  private def drawBackground() {
		background.setImage(buildBackground)
  }

  private def renderPaint(color:Color, gridX:Int, gridY:Int) {
    val g = background.getImage.getGraphics
		val x = gridX * squareSize
		val y = gridY * squareSize
		g.setColor(color)
		g.fillRect(x + 1, y + 1, squareSize-1, squareSize-1)
  }

  override def paint() {
    environment.paint(state)
    renderPaint(Color.YELLOW, state.gridX, state.gridY)
  }

  def syncToState(state:State) {
    this.state = state
		transform = new AffineTransform()
		transform.translate(state.gridX * 50, state.gridY * 50)
		transform.rotate(state.directionIndex * (java.lang.Math.PI/2), 25, 25)
		robot.setTransform(transform)
	}

  def syncEnvironment() {
    drawBackground()
    for (x <- 0 to environment.sizeX-1; y <- 0 to environment.sizeY-1)
      if (environment.isPainted(x,y)) {
        renderPaint(Color.YELLOW, x, y)
      }
    environment.obstructed.foreach { square =>
      renderPaint(Color.BLACK, square._1, square._2)
    }
  }

  override def executeMoveForward() = {
	  super.executeMoveForward()
	  transform.translate(0, -50)
    executeAnimation(500)
	}
 
	override def turnRight() = {
	  super.turnRight()
	  transform.rotate(java.lang.Math.PI/2, 25, 25)
	  executeAnimation(1000)
    }
  
	override def turnLeft() = {
	  super.turnLeft()
	  transform.rotate(-java.lang.Math.PI/2, 25, 25)
	  executeAnimation(1000)
	}

  def reset() {
    environment.reset()
    resetCallStack()
  }

  canvas.setPreferredSize(new Dimension(environment.sizeX * squareSize, environment.sizeY * squareSize)) 
  drawBackground()
  layer.addChild(background)
  canvas.setPanEventHandler(null);
  syncEnvironment()

  background.addChild(robot)
}
