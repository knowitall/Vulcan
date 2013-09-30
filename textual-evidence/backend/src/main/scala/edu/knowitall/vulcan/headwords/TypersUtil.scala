package edu.knowitall.vulcan.headwords

import edu.washington.cs.knowitall.tool.tokenize.Token
import edu.washington.cs.knowitall.collection.immutable.Interval

/**
 * Created with IntelliJ IDEA.
 * User: niranjan
 * Date: 2/14/13
 * Time: 3:49 PM
 * To change this template use File | Settings | File Templates.
 */
object TypersUtil {

  def main(args:Array[String]){
    val testTokens = new Token("test", 0)::new Token("another", 5)::Nil
    println(startEnd(testTokens))
  }

  def startingOffset(tokens:Seq[Token]) = tokens.head.offset

  def span(tokens:Seq[Token]): Interval = {
    val (start:Int, end:Int) = startEnd(tokens)
    Interval.open(start, end)
  }

  def startEnd(tokens:Seq[Token]): (Int, Int) = {
    val start = tokens.map(t => t.interval.start).min
    val end = tokens.map(t => t.interval.end).max
    (start, end)
  }
}
