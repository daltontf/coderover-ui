package tfd.coderover.ui

import _root_.tfd.coderover.{Environment, State}

class GUIEnvironment(
        sizeX:Int,
        sizeY:Int,
        obstructed:Set[(Int,Int)],
        prePainted:Set[(Int,Int)],
        val targetLocation:Option[(Int,Int)],
        val visibleEntities:Map[String, Set[(Int,Int)]],
        val hiddenEntities:Map[String, Set[(Int, Int)]]
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

  private def findEntity(entity: String, index:Int):Option[(Int, Int)] =
    for (set <- visibleEntities.get(entity);
         pair <- set.toIndexedSeq.lift(index)) yield (pair)

  override def distanceX(entity:String, index:Int, x:Int, y:Int):Option[Int] =
    findEntity(entity, index) match {
      case Some((thatX, _)) => Some(x - thatX)
      case None => None
  }

  override def distanceY(entity:String, index:Int, x:Int, y:Int):Option[Int] =
     findEntity(entity, index) match {
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

  override def count(entity:String) = visibleEntities.get(entity).map(_.size)
  
  def reset() {
    painted = Array.ofDim[Boolean](sizeX, sizeY)
    prePainted.foreach { tuple => paint(tuple._1, tuple._2) }
  }

  reset()
}
