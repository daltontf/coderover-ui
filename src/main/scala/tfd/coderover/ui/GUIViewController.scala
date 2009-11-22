package tfd.coderover.ui

import java.awt.{Color, Dimension, Graphics}
import java.awt.image.BufferedImage
import java.awt.geom.AffineTransform

import edu.umd.cs.piccolo.PCanvas;
import edu.umd.cs.piccolo.nodes.PImage;

import tfd.coderover.{Controller}

class GUIViewController(var squareSize:Int, var environment:GUIEnvironment) extends Controller(environment) {
	private var transform:AffineTransform = _
      
	private val colorMap = Map(
								0 -> Color.BLACK,
								1 -> Color.GREEN,
								2 -> Color.YELLOW,
								3 -> Color.BLUE,
								4 -> Color.RED,
								5 -> Color.WHITE,
								6 -> Color.CYAN,
								7 -> Color.MAGENTA,
								8 -> Color.ORANGE
							)
  
  val canvas = new PCanvas()  
   
  canvas.setPreferredSize(new Dimension(environment.sizeX * squareSize, environment.sizeY * squareSize)) 
    
  private val layer = canvas.getLayer() 
  
  private def executeAnimation(duration:Int) {
		robot.animateToTransform(transform, duration ) 
		Thread.sleep(duration)
	}
  
  def makePaintedImage(width:Int, height:Int, painter:Graphics => Unit) = {
		val image = new BufferedImage(width, height, BufferedImage.TYPE_4BYTE_ABGR)
    	painter(image.getGraphics)
    	image
  }
  
  def buildBackground() = makePaintedImage(environment.sizeX * squareSize + 1, environment.sizeY * squareSize + 1, { g:Graphics => 
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
  	})

  def drawBackground() {
		background.setImage(buildBackground)
  }
    
  val background = new PImage()
  
  drawBackground()
  
  layer.addChild(background)
                                
  canvas.setPanEventHandler(null);
  val robot = new PImage(makePaintedImage(squareSize,squareSize, { g:Graphics => 
        	g.setColor(Color.RED)
        	val oneHalf = squareSize/2;
        	val oneFifth = squareSize/5;
        	val fourFifths = oneFifth * 4
          	g.drawLine(oneHalf, oneFifth, oneFifth, fourFifths)
            g.drawLine(oneFifth, fourFifths, fourFifths, fourFifths)
            g.drawLine(fourFifths, fourFifths, oneHalf, oneFifth)
  }))
  background.addChild(robot)  
  
  def paint(state:State, color:Int) {
    if (colorMap.contains(color)) {
    	val g = background.getImage.getGraphics
		val x = state.gridX * squareSize
		val y = state.gridY * squareSize
		g.setColor(colorMap(color))
		g.fillRect(x + 1, y + 1, squareSize-1, squareSize-1)
    } else {
    	state.fail(new Abend("Invalid Paint Color specified:" + color) { })
    }
  }
  
  	def syncToState(state:State) {
		transform = new AffineTransform()
		transform.translate(state.gridX * 50, state.gridY * 50)
		transform.rotate(state.directionIndex * (java.lang.Math.PI/2), 25, 25)
		robot.setTransform(transform)
	}
	
	override def moveForward(state:State) = {
	  super.moveForward(state)
	  transform.translate(0, -50)
      executeAnimation(500)
	}
 
	override def turnRight(state:State) = {
	  super.turnRight(state)
	  transform.rotate(java.lang.Math.PI/2, 25, 25)
	  executeAnimation(1000)
    }
  
	override def turnLeft(state:State) = {
	  super.turnLeft(state)
	  transform.rotate(-java.lang.Math.PI/2, 25, 25)
	  executeAnimation(1000)
	}
 
	override def paint(color:Int, state:State) = {
	  super.paint(color, state)
   }
}
