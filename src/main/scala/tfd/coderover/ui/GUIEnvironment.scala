package tfd.coderover.ui

import _root_.tfd.coderover.{Environment, State}

class GUIEnvironment(
        sizeX:Int = 10,
        sizeY:Int = 10,
        obstructed:Set[(Int,Int)] = Set.empty[(Int,Int)],
        prePainted:Set[(Int,Int)] = Set.empty[(Int,Int)],
        val targetLocation:Option[(Int,Int)] = None,
        val visibleEntities:Map[String, Set[(Int,Int)]] = Map.empty[String, Set[(Int,Int)]],
        val hiddenEntities:Map[String, Set[(Int, Int)]] = Map.empty[String, Set[(Int,Int)]]
  ) extends Environment(sizeX, sizeY, obstructed) {
  import Math._

  protected var painted:Array[Array[Boolean]] = _
  
  def paint(x:Int, y:Int) {
    painted(x)(y) = true
  }

  override def paint(state:State) =  paint(state.gridX, state.gridY)

  def isPainted(x:Int, y:Int) = painted(x)(y)

  override def isPainted(x:Int, y:Int, state:State):Boolean =
    if (x < 0 || y < 0 || x >= sizeX || y >= sizeY) {
      false
    } else {
      isPainted(x, y)
    }

  private[this] def distance(x1:Int, y1:Int, x2:Int, y2:Int) = {
    sqrt(abs(x2 - x1)^2 + abs(y2 - y1)^2)
  }

  private[this] def closestVisibleEntity(entityString:String, x:Int, y:Int):Option[(Int,Int)] = {
    var closest:Option[(Int,Int)] = None
    var closestDistance = 999999999.9
    for (locs <- visibleEntities.get(entityString);
         loc <- locs.iterator) {
      val locDistance = distance(x, y, loc._1, loc._2)
      if (locDistance  < closestDistance) {
        closest = Some(loc)
        closestDistance = locDistance
      }
    }
    closest
  }

  override def distanceX(entity:String, state:State):Option[Int] =
    closestVisibleEntity(entity, state.gridX, state.gridY) match {
      case Some((x, _)) => Some(state.gridX - x)
      case None => None
  }

  override def distanceY(entity:String, state:State):Option[Int] =
    closestVisibleEntity(entity, state.gridX, state.gridY) match {
      case Some((_, y)) => Some(state.gridY - y) 
      case None => None
  }

  override def adjacent(entity:String, state:State) = {
    val entityLocations = hiddenEntities.getOrElse(entity, Set.empty) ++ visibleEntities.getOrElse(entity, Set.empty)
    entityLocations.contains((state.gridX - 1, state.gridY)) ||
    entityLocations.contains((state.gridX + 1, state.gridY)) ||
    entityLocations.contains((state.gridX, state.gridY - 1)) ||
    entityLocations.contains((state.gridX, state.gridY + 1))
  }
  
  def reset() {
    painted = Array.ofDim[Boolean](sizeX, sizeY)
    prePainted.foreach { tuple => paint(tuple._1, tuple._2) }
  }

  reset()
}
