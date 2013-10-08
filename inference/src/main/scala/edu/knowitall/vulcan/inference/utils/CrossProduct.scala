package edu.knowitall.vulcan.inference.utils

/**
 * Created with IntelliJ IDEA.
 * User: niranjan
 * Date: 10/4/13
 * Time: 8:37 PM
 * To change this template use File | Settings | File Templates.
 */


import scala.collection.JavaConversions._

/**
 * Created with IntelliJ IDEA.
 * User: niranjan
 * Date: 9/6/12
 * Time: 10:51 AM
 * To change this template use File | Settings | File Templates.
 */
// Courtesy Randall Schulz
// Code based on: http://stackoverflow.com/questions/2725682/cross-product-of-2-sets-in-scala
class Crossable[X](collA:Traversable[X]) {

  def x[Y](collB: Traversable[Y]): Traversable[(X, Y)] = {
    for (a <- collA; b <- collB) yield (a, b)
  }

  def cross[Y](collB: Traversable[Y]): Traversable[(X, Y)] = {
    for (a <- collA; b <- collB) yield (a, b)
  }
}

object Crossable
{
  implicit def trav2Crossable[E1](es1: Traversable[E1]): Crossable[E1] = new Crossable[E1](es1)
}