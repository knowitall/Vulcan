package edu.knowitall.vulcan.common

sealed trait PathPart {
  def pathString: String
}

case class Path(parts: Seq[PathPart]) extends PathPart {

  val sep = "/"

  def pathString: String = {
    parts map { _.pathString } mkString(sep)
  }

  def /(path: Path): Path = {
    Path(parts ++ path.parts)
  }

  def rooted: Path = {
    Path(parts :+ Path.Root)
  }
}

object Path {

  val Empty = Path(Seq())
  
  val Arg1 = Path(Seq(new PathPart() { val pathString = "a1" }))
  val Rel  = Path(Seq(new PathPart() { val pathString = "rel" }))
  val Arg2 = Path(Seq(new PathPart() { val pathString = "a2" }))
  val Star = Path(Seq(new PathPart() { val pathString = "*" }))

  private case object Root extends PathPart { val pathString = "$" }
  
  private case class Leaf(val pathString: String) extends PathPart

  def of(hierarchy: PathPart*) : Path = {
    Path(hierarchy)
  }

  def forTerm(term: String, path: Path) : Path = {
    Path(Seq(Leaf(term))) / path
  }
}
    





