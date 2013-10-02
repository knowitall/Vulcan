package edu.knowitall.vulcan.solr

import java.io.Reader
import java.util.{Map => JMap}

import org.apache.lucene.analysis.Tokenizer
import org.apache.lucene.analysis.util.TokenizerFactory
import org.apache.lucene.util.AttributeSource.AttributeFactory

class TupleTokenizerFactory(params: JMap[String,String]) extends TokenizerFactory(params) {
  
  def create(unused: AttributeFactory, input: Reader) : Tokenizer = {
    new TupleTokenizer(input)
  }
}
