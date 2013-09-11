package edu.knowitall.vulcan.openie

import edu.knowitall.tool.stem.Lemmatized
import edu.knowitall.tool.chunk.ChunkedToken

/***
 * A concrete instance of an extractions.
 */
case class Instance(confidence: Double, sentence: String, extraction: Extraction, postags: Seq[Lemmatized[ChunkedToken]]) {
  def conf = confidence
  def extr = extraction

  def postagsString = 
    postags.map(lt => Seq(lt.lemma, lt.token.chunk, lt.token.postag).mkString("(", ", ", ")")).mkString(", ")

  override def toString = f"$conf%1.2f $extraction"
}
