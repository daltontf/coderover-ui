package tfd.coderover.ui

import _root_.tfd.coderover.{BoundedEnvironment}

class GUIEnvironment(sizeX:Int, sizeY:Int, val targetLocation:Option[(Int,Int)]) extends BoundedEnvironment(sizeX, sizeY) {
  def this(sizeX:Int, sizeY:Int) = this(sizeX, sizeY, None)	

  private class Square {
	  var paint:Option[Int] = None
  }
  
  private var grid = new Array[Array[Square]](sizeX, sizeY)
  
  override def paint(color:Int, state:State) { 
	  var square = grid(state.gridX)(state.gridY)
	  if (square == null) {
		  square = new Square()
		  grid(state.gridX)(state.gridY) = square
	  }
	  square.paint = Some(color)
  }

  override def isPainted(x:Int, y:Int) = {
	  var square = grid(x)(y)
	  if (square == null) {
		  false
	  } else {
	      square.paint != None
	  }
  }
  
  def reset() {
    grid = new Array[Array[Square]](sizeX, sizeY)
  }
}
