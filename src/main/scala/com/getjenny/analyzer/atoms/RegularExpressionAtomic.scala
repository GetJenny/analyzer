package com.getjenny.analyzer.atoms

import com.getjenny.analyzer.expressions.{AnalyzersDataInternal, Result}

/**
  * Created by mal on 20/02/2017.
  */

/** Counts the occurrences of a pattern into a string
  *
  * @param arguments regular expression
  */
class RegularExpressionAtomic(arguments: List[String], restricted_args: Map[String, String]) extends AbstractAtomic {
  /**
    * A Regex
    */
  val re = arguments.headOption match {
    case Some(t) => t
    case _ => throw ExceptionAtomic("RegularExpressionAtomic: must have one argument")
  }
  override def toString: String = "regex(\"" + re + "\")"
  val isEvaluateNormalized: Boolean = false
  private[this] val rx = re.r

  def evaluate(query: String, data: AnalyzersDataInternal = AnalyzersDataInternal()): Result = {
    val score = rx.findAllIn(query).toList.length
    Result(score = score)
  }
}
