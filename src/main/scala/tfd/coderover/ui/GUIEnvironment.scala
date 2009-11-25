package tfd.coderover.ui

import _root_.tfd.coderover.{BoundedEnvironment}

class GUIEnvironment(sizeX:Int, sizeY:Int, val targetLocation:Option[(Int,Int)]) extends BoundedEnvironment(sizeX, sizeY) {
  def this(sizeX:Int, sizeY:Int) = this(sizeX, sizeY, None)	

  private class Square {
	  var painted = false
  }
  
  private var grid:Array[Array[Square]] = _
  
  override def paint(x:Int, y:Int) {
	  var square = grid(x)(y)
	  if (square == null) {
		  square = new Square()
		  grid(x)(y) = square
	  }
	  square.painted = true
  }

  override def isPainted(x:Int, y:Int) = {
	  var square = grid(x)(y)
    if (square == null) {
		  false
	  } else {
	      square.painted
	  }
  }
  
  def reset() {
    grid = new Array[Array[Square]](sizeX, sizeY)
  }

  reset()
}
