package tfd.coderover.ui

import java.awt.Color

import java.awt.geom.AffineTransform

import edu.umd.cs.piccolo.PNode
import edu.umd.cs.piccolo.nodes.PImage

class GUIEvaluator(robot:PNode, background:PImage, environment:Environment) extends Evaluator(environment) {
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
  
	private def executeAnimation(duration:Int) {
		robot.animateToTransform(transform, duration ) 
		Thread.sleep(duration)
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
	  if (colorMap.contains(color)) {
		  val g = background.getImage.getGraphics
		  val x = state.gridX * 50
		  val y = state.gridY * 50
		  g.setColor(colorMap(color))
		  g.fillRect(x + 1, y + 1, 49, 49)
	  } else {
	    // TODO fail
	  }
	}
}

