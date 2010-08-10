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
  ) extends Environment(sizeX, sizeY) {
  import Math._
   
  protected var painted:Array[Array[Boolean]] = _
  
  override def paint(x:Int, y:Int) {
    painted(x)(y) = true
  }

  override def isPainted(x:Int, y:Int):Boolean =
    if (x < 0 || y < 0 || x >= sizeX || y >= sizeY) {
      false
    } else {
      painted(x)(y)
    }

  override def isObstructed(x: Int, y: Int) = obstructed.contains((x,y))

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

  override def distanceX(entity:String, x:Int, y:Int):Option[Int] =
    closestVisibleEntity(entity, x, y) match {
      case Some((thatX, _)) => Some(x - thatX)
      case None => None
  }

  override def distanceY(entity:String, x:Int, y:Int):Option[Int] =
    closestVisibleEntity(entity, x, y) match {
      case Some((_, thatY)) => Some(y - thatY) 
      case None => None
  }

  override def adjacent(entity:String, x:Int, y:Int) = {
    val entityLocations = hiddenEntities.getOrElse(entity, Set.empty) ++ visibleEntities.getOrElse(entity, Set.empty)
    entityLocations.contains((x - 1, y)) ||
    entityLocations.contains((x + 1, y)) ||
    entityLocations.contains((x, y - 1)) ||
    entityLocations.contains((x, y + 1))
  }
  
  def reset() {
    painted = Array.ofDim[Boolean](sizeX, sizeY)
    prePainted.foreach { tuple => paint(tuple._1, tuple._2) }
  }

  reset()
}
