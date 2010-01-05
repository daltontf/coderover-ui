package tfd.coderover.ui

import _root_.tfd.coderover.{GridLocationOutOfBounds, Environment, State}

class GUIEnvironment(sizeX:Int, sizeY:Int, val targetLocation:Option[(Int,Int)]) extends Environment(sizeX, sizeY) {
  def this(sizeX:Int, sizeY:Int) = this(sizeX, sizeY, None)	

  private class Square {
	  var painted = false
  }
  
  private var grid:Array[Array[Square]] = _

  def paint(x:Int, y:Int) {
    var square = grid(x)(y)
	  if (square == null) {
		  square = new Square()
		  grid(x)(y) = square
	  }
	  square.painted = true
  }

  override def paint(state:State) =  paint(state.gridX, state.gridY)

  def isPainted(x:Int, y:Int) = {
    var square = grid(x)(y)
    if (square == null) {
		  false
	  } else {
	      square.painted
	  }
  }

  override def isPainted(x:Int, y:Int, state:State):Boolean =
    if (x < 0 || y < 0 || x >= sizeX || y >= sizeY) {
      state.fail(GridLocationOutOfBounds)
      false
    } else {
      isPainted(x, y)
    }
  
  def reset() {
    grid = new Array[Array[Square]](sizeX, sizeY)
  }

  reset()
}
