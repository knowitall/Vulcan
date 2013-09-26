package edu.knowitall.vulcan.common

/**
 * Extraction represents a Tuple which has been extracted from some source text.
 *
 * It wraps a Tuple and includes some information about the source text:
 *  - the source sentence text (as well as postags/lemmas/chunks)
 *  - corpus the sentence came from
 *  - confidence of the extraction
 *  - id of the extraction (combining corpus and a counter makes sense)
 *  - id of the corpus the extraction came from
 */
case class Extraction(tuple: Tuple,
                      sentence: String,
                      sentenceDetails: Seq[Term],
                      confidence: Double,
                      id: String,
                      corpus: String)
