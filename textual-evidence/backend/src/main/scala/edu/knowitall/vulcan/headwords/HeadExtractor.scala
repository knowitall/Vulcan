package edu.knowitall.vulcan.headwords

/**
 * Created with IntelliJ IDEA.
 * User: niranjan
 * Date: 2/14/13
 * Time: 2:26 PM
 * To change this template use File | Settings | File Templates.
 */


import collection.mutable.ArrayBuffer
import edu.knowitall.tool.postag.PostaggedToken
import collection.mutable
import scala.Predef._
import scala.Some

import edu.washington.cs.knowitall.tool.stem.MorphaStemmer
import io.Source
import org.slf4j.LoggerFactory

/**
 * Created by IntelliJ IDEA.
 * User: niranjan
 * Date: 11/26/11
 * Time: 10:20 PM
 * To change this template use File | Settings | File Templates.
 */

object HeadExtractor{

  def main(args:Array[String]){
    val headExtractor = new HeadExtractor(args(0))
    val testCasesFile = args(1)
    Source.fromFile(testCasesFile).getLines.foreach(line => {
      val splits = line.split("\t")
      val argText = splits(0)
      val argPosTags = splits(1)
      headExtractor.argumentHead(argText, argPosTags) match {
        case Some(head:Seq[PostaggedToken]) => println("Head(%s)=%s".format(argText, head.map(_.string).mkString(" ")))
        case None => println("Head(%s)=%s".format(argText, ""))
      }
    })
  }

}
class HeadExtractor(wnHome:String) {

  val logger = LoggerFactory.getLogger(this.getClass)

  val stopPosTagsString = "MD,JJ,JJR,JJS,RB,RBR,RBS,CC,UH,PRP,PRP$,DT,WP,WP$,WRB,CD"
  val stopTagsForRelation = stopPosTagsString.split(",").toSet
  val stopwordsForRelations = "has,have,had,did,do".split(",").toSet

  def relHead(tokens:Seq[PostaggedToken]) = {
    val outTokens = tokens.filter(token => !(stopTagsForRelation.contains(token.postag) ||
                                             stopwordsForRelations.contains(token.string)))
    if(outTokens.isEmpty) None else Some(outTokens)
  }

  val pronouns = ("you"::"i"::"he"::"she"::"they"::"it"::"them"::"him"::"her"::"whom"::Nil).toSet

  def replacePronouns(text: String): String = {
    if (pronouns.contains(text)){
      return "[PRN]"
    }else{
      return text
    }

  }

  val whwords = ("what"::"which"::"who"::"whose"::"that"::"where"::"when"::Nil).toSet
  def isConjunction(token:PostaggedToken) = {
    token.postag.startsWith("CC") || token.postag.startsWith("W") || whwords.contains(token.string)
  }
  def isPreposition(token:PostaggedToken) = token.postag.startsWith("IN")

  def removeTokensAfterConjunctionsOrPrepositions(tokens: Seq[PostaggedToken]): Seq[PostaggedToken] = {
    var firstConjPrepIndex = tokens.indexWhere(token => isConjunction(token) || isPreposition(token))
    var firstNounIndex = tokens.indexWhere(token => token.isNoun)
    if(firstConjPrepIndex > 0 && firstNounIndex < firstConjPrepIndex) {
      val subTokens = tokens.take(firstConjPrepIndex)
      subTokens
    }else{
      tokens
    }

  }


  //val punctuationRe = """[^a-zA-Z0-9]""".r
  val leadingModPatterns = """^(DT|CD|(DT*) JJ|JJ|RBS) of""".r

  def dropLeadingModifierOfPatterns(tokens:Seq[PostaggedToken]):Seq[PostaggedToken] = {
    val leadingOfIndex = tokens.indexWhere(token => token.string.equals("of"))
    if(leadingOfIndex > 0){
      val posTokenString = tokens.take(leadingOfIndex+1).map(token => if(token.string.equals("of")) "of" else token.postag).mkString(" ")
      if(leadingModPatterns.findFirstIn(posTokenString) != None){
        return tokens.slice(leadingOfIndex+1,tokens.size)
      }
    }
    return tokens
  }


  def truncateBeforeRelativeClause(intokens:Seq[PostaggedToken]) = {
    val index = intokens.indexWhere(p => p.postag.startsWith("W"))
    if(index > 0) intokens.take(intokens.indexWhere(p => p.postag.startsWith("W"))) else intokens
  }

  def removeTokensBeforeAppositive (tokens: Seq[PostaggedToken]): Seq[PostaggedToken] = {
    val apostropheIndex = tokens.indexWhere(token => token.string.equals("POS") || token.postag.equals("POS"))
    if(apostropheIndex > 0 && (apostropheIndex+1) < tokens.size){
      tokens.drop(apostropheIndex+1)
    }else{
      tokens
    }
  }
  val punctuationOtherThanDotRe = """[^a-zA-Z0-9.]""".r
  def removeTokensWithPunctuation(tokens:Seq[PostaggedToken]): Seq[PostaggedToken] = {
    tokens.filter(p => punctuationOtherThanDotRe.findFirstIn(p.string) == None)
  }

  val punctuationOtherThanDotDollarRe = """[^a-zA-Z0-9.$]""".r
  val alphaNumericDotOrDollarRe = """[^a-zA-Z0-9.$]""".r
  //If the entire token is a punctuation then
  def removeTokensAfterPunctuation(tokens: Seq[PostaggedToken]): Seq[PostaggedToken] = {
    val puncIndex = tokens.indexWhere(token => alphaNumericDotOrDollarRe.findFirstIn(token.string) == None)
    if (puncIndex > 0) {
      tokens.take(puncIndex)
    }else{
      tokens
    }
  }

  var wnTypers = new mutable.HashMap[Thread, WordNetTyper] with mutable.SynchronizedMap[Thread, WordNetTyper]


  def findNPofNP(tokens:Seq[PostaggedToken]):Seq[PostaggedToken] = {

    val leadingOfIndex = tokens.indexWhere(token => token.string.equals("of") || token.string.equals("in"))
    if(leadingOfIndex > 0){
      /**
       * First step is to find the head noun of an argument phrase, as follows:
          If the phrase begins with "NP of NP", check the WN type and hypernyms of the first NP.
          If the first NP has type { number[n2], group[n1], quantity[n1], part[n1],amount[n1] }, then remove "NP of".
          If the phrase begins with "Adj of NP", then remove "Adj of NP".  (example:"some of NP", "any of NP").
          Find first token of remaining phrase that is not in { Noun, Adj, Determiner}. Truncate phrase at that word.
       */
      val posTokenString = tokens.take(leadingOfIndex+1).map(token => {
        if(token.string.equals("of") || token.string.equals("in")) "of" else token.postag
      }).mkString(" ")

      if(leadingModPatterns.findFirstIn(posTokenString) != None){
        return tokens.drop(leadingOfIndex+1)
      }

      var leadingNPs = tokens.take(leadingOfIndex).filter(x => x.isNoun)
      var trailingNPs = tokens.drop(leadingOfIndex+1).filter(x => x.isNoun)
      if (leadingNPs.find(x => !x.isProperNoun) == None){
        return tokens
      }
      def getWordnetTyper = new WordNetTyper(wnHome, "", 1::Nil, true, true)
      val wnTyper = wnTypers.getOrElseUpdate(Thread.currentThread(), getWordnetTyper)
      leadingNPs.filter(x => !x.isProperNoun).foreach(x => if (wnTyper.isGroupQuantityAmountNumberOrPart(x)) return trailingNPs)
      return leadingNPs
    }
    return tokens
  }



  private def stem(intoken: String, posTag: String): String = MorphaStemmer.stem(intoken, posTag)


  def lemmatizedArgumentHead(argText:String, argPosTagString:String):Option[String] = {
    try{
      import edu.knowitall.vulcan.headwords.Pairable._
      val postaggedTokens = (argPosTagString.split(" ").toSeq pairElements argText.split(" ")).map(x => new PostaggedToken(x._1, x._2, offset=0))
      lemmatizedArgumentHead(postaggedTokens)
    }catch{
      case e:Exception => {
        println("Caught exception processing arg: " + argText + " with postags: " + argPosTagString)
        e.printStackTrace()
        None
      }
    }
  }

  def lemmatize(tokens: Seq[PostaggedToken]) = tokens.map(token => new PostaggedToken(token.postag, stem(token.string, token.postag), token.offset))//stem(token.string, token.postag))

  def lemmatizedArgumentHead(tokens:Seq[PostaggedToken]):Option[String] = argumentHead(tokens) match {
    case Some(headTokens:Seq[PostaggedToken]) => Some(lemmatize(headTokens).mkString(" "))
    case None => None
  }

  def argumentHead(argText:String, argPosTagString:String):Option[Seq[PostaggedToken]] = {
    try{
      import edu.knowitall.vulcan.headwords.Pairable._
      val postaggedTokens = (argPosTagString.split(" ").toSeq pairElements argText.split(" ")).map(x => new PostaggedToken(x._1, x._2, offset=0))
      argumentHead(postaggedTokens)
    }catch{
      case e:Exception => {
        println("Caught exception processing arg: " + argText + " with postags: " + argPosTagString)
        e.printStackTrace()
        None
      }
    }
  }



  def argumentHead(tokens:Seq[PostaggedToken]):Option[Seq[PostaggedToken]] = {

    var subTokens = truncateBeforeRelativeClause(tokens)

    var returnTokens = findNPofNP(subTokens)
    if (!returnTokens.isEmpty){
      subTokens = returnTokens
    }

    returnTokens = removeTokensBeforeAppositive(subTokens) //Jan 30    -- should fix Fazil 's rank to rank instead of Fazil.
    if (!returnTokens.isEmpty){
      subTokens = returnTokens
    }

    returnTokens = removeTokensAfterPunctuation(subTokens)
    if (!returnTokens.isEmpty){
      subTokens = returnTokens
    }

    returnTokens = removeTokensAfterConjunctionsOrPrepositions(subTokens) //Check this. Why isn't this filtering out which led to....
    if(!returnTokens.isEmpty){
      subTokens = returnTokens
    }

    def allowedToken(p:PostaggedToken) = !p.postag.startsWith("W") &&
      (p.isNoun || p.isAdjective || p.isVerbGerund || p.postag.equals("CD") ||
        p.postag.equals("DT") || p.string.equals("the") || p.string.equals("a") || p.string.equals("an"))

    def contentToken(p:PostaggedToken): Boolean = p.isNoun || p.isPronoun || p.postag.equals("CD")

    val truncateIndex = subTokens.indexWhere(p => !allowedToken(p))
    if (truncateIndex > 0 && subTokens.take(truncateIndex).find(p => (p.isPronoun || p.isNoun)) != None){//&& returnTokens.find(p => p.isNoun).isDefined){
      subTokens = subTokens.take(truncateIndex)
    }else if(subTokens.find(p => contentToken(p)) == None){
      return None
    }

    /**
     * If NNPS* NNPS* -> return full sequence. (Saudi Arabia --> Saudi Arabia)
     * If NNPS* NN  -> return last NN (Saudi exile -> exile)
     * If NN NN -> return last NN (air plane --> air plane)
     *
     */

    return findLastNounAndNormalized(subTokens)

  }


  def findLastNounAndNormalized(subTokens: Seq[PostaggedToken]): Option[Seq[PostaggedToken]] = {

    assert(subTokens.size > 0, " Sub tokens cannot be zero.")
    //If only one token left return the stemmed version.

    //If no noun exists return the stemmed version of the entire string.
    if(subTokens.find(token => (token.isNoun || token.isPronoun)) == None){
      //logger.error("Failed to find nouns in string: " + subTokens)
      return Some(subTokens.last::Nil)
    }

    //If there is only one noun left. Return.
    if(subTokens.size < 2){
      return Some(subTokens)
    }

    def findLastPosSequence(tokens:Seq[PostaggedToken], posTester:PostaggedToken => Boolean):Option[Seq[PostaggedToken]] = {
      val nounSequence = tokens.zipWithIndex.filter(x => posTester(x._1)).reverse.toSeq
      if(nounSequence.isEmpty) return None
      var prevIndex = nounSequence.head._2
      val lastSeq = new ArrayBuffer[PostaggedToken]
      nounSequence.foreach(n => {
        val curIndex = n._2
        if ((prevIndex - curIndex) > 1){
          return Some(lastSeq.reverse)
        }
        prevIndex = curIndex
        lastSeq += n._1
      })
      return Some(lastSeq.reverse)
    }
    def isNoun(x:PostaggedToken) = x.isNoun || x.isPronoun
    def isProperNoun(x:PostaggedToken) = x.isProperNoun

    findLastPosSequence(subTokens, isNoun) match {
      case Some(lastNounSeq:Seq[PostaggedToken]) => {
        val lastNoun = lastNounSeq.last
        if (lastNoun.isCommonNoun){
          return Some(Seq(lastNoun))
        }else{
          return findLastPosSequence(lastNounSeq, isProperNoun)
        }
      }
      case _ => None
    }
  }



}


