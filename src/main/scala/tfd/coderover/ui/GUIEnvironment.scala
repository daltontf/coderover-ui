package tfd.coderover.ui

import _root_.tfd.coderover.{GridLocationOutOfBounds, Environment, State}

class GUIEnvironment(
        sizeX:Int = 10,
        sizeY:Int = 10,
        obstructed:Set[(Int,Int)] = Set.empty[(Int,Int)],
        prePainted:Set[(Int,Int)] = Set.empty[(Int,Int)],
        val targetLocation:Option[(Int,Int)] = None
  ) extends Environment(sizeX, sizeY, obstructed) {
  
  protected var painted:Array[Array[Boolean]] = _
  
  def paint(x:Int, y:Int) {
    painted(x)(y) = true
  }

  override def paint(state:State) =  paint(state.gridX, state.gridY)

  def isPainted(x:Int, y:Int) = painted(x)(y)

  override def isPainted(x:Int, y:Int, state:State):Boolean =
    if (x < 0 || y < 0 || x >= sizeX || y >= sizeY) {
      state.fail(GridLocationOutOfBounds)
      false
    } else {
      isPainted(x, y)
    }
  
  def reset() {
    painted = Array.ofDim[Boolean](sizeX, sizeY)
    prePainted.foreach { tuple => paint(tuple._1, tuple._2) }
  }

  reset()
}
