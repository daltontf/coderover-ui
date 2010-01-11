package tfd.coderover.ui

import _root_.tfd.coderover.{GridLocationOutOfBounds, Environment, State}

class GUIEnvironment(sizeX:Int, sizeY:Int, obstructed:Set[(Int,Int)], prePainted:Set[(Int,Int)], val targetLocation:Option[(Int,Int)]) extends Environment(sizeX, sizeY, obstructed) {
  def this(sizeX:Int, sizeY:Int) = this(sizeX, sizeY, Set.empty[(Int,Int)], Set.empty[(Int,Int)], None)

  def this(sizeX:Int, sizeY:Int, targetLocation:Option[(Int,Int)]) = this(sizeX, sizeY, Set.empty[(Int,Int)], Set.empty[(Int,Int)], targetLocation)

  protected var painted:Array[Array[Boolean]] = _

  def paint(x:Int, y:Int) {
    painted(x)(y) = true
  }

  override def paint(state:State) =  paint(state.gridX, state.gridY)

  def isPainted(x:Int, y:Int) = {
    var value = painted(x)(y)
    if (value == null) {
		  false
	  } else {
	    value
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
    painted = new Array[Array[Boolean]](sizeX, sizeY)
    prePainted.foreach { tuple => paint(tuple._1, tuple._2) }
  }

  reset()
}
