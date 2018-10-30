package com.getjenny.analyzer.atoms

import com.getjenny.analyzer.expressions.{AnalyzersDataInternal, Result}
import com.getjenny.analyzer.util._

import scala.util.Try
import scala.util.control.NonFatal

/**
  * Created by angelo on 26/06/17.
  */

/** Analyzer for the extraction of dates from queries
  * @param arguments
  */
class MatchDateDDMMYYYYAtomic(val arguments: List[String], restricted_args: Map[String, String]) extends AbstractAtomic {
  val prefix = arguments.headOption match {
    case Some(t) => t
    case _ => throw ExceptionAtomic("MatchDateDDMMYYYYAtomic: must have one argument")
  }
  override def toString: String = "matchDateDDMMYYYY(" + prefix + ")"
  val isEvaluateNormalized: Boolean = true
  val regex = """[""" + prefix + """day,""" + prefix + """month,""" + prefix + """year]""" +
    """(?:(?:[^0-9]+|\A)(0[1-9]|[12][0-9]|3[01])(?:[- \/\.])(0[1-9]|1[012])(?:[- \/\.])((?:19|20)\d\d)(?:[^0-9]+|$))"""

  /** PatternExtractionRegex is a pattern extraction utility class */
  val regexExtractor = new PatternExtractionRegex(regex)

  /** Extract one or more dates from the query. If the query contains the pattern it returns a score = 1.0 and
    *   put the pattern into the extracted_variables dictionary.
    *   The regular expression extracts dates like the following: 10.12.2017 or 10-12-2017 or 10/11/2017
    *
    * @param query the user query
    * @param data the dictionary of variables (not used in this analyzer)
    * @return Result with 1.0 the date on extracted_variables if the pattern matches, score = 0.0 otherwise
    */
  def evaluate(query: String, data: AnalyzersDataInternal = AnalyzersDataInternal()): Result = {
    val res = Try(Result(score = 1.0,
        AnalyzersDataInternal(traversed_states = data.traversed_states, extracted_variables = regexExtractor.evaluate(query))
      )) recover {
      case _: PatternExtractionNoMatchException =>
        Result(score=0)
      case NonFatal(e) =>
        throw ExceptionAtomic("Parsing of regular expression specification(" + regex + "), query(" + query + ")", e)
    }
    res.get
  }
}
