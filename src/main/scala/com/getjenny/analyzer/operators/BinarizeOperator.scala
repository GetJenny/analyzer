package com.getjenny.analyzer.operators

import com.getjenny.analyzer.entities.{AnalyzersDataInternal, Result, StateVariables}
import com.getjenny.analyzer.expressions._
import scalaz.Scalaz._

import scala.math.Ordering.Double.equiv

/** Binarize Operator
  *
  * It can only take one argument and return 1.0 if the expression is > 0 returns 0.0 otherwise
  *
  * Created by Angelo Leto on 19/10/2018.
  */

class BinarizeOperator(child: List[Expression]) extends AbstractOperator(child: List[Expression]) {
  require(child.length <= 1, "BinarizeOperator can only have one Expression")
  override def toString: String = "binarize(" + child + ")"
  def add(e: Expression, level: Int = 0): AbstractOperator = {
    if (level === 0) {
      if (child.nonEmpty)
        throw OperatorException("BinarizeOperator: trying to add more than one expression.")
      new BinarizeOperator(e :: child)
    } else child.headOption match {
      case Some(c: AbstractOperator) =>
        child.tailOption match {
          case Some(tail) =>
            if (tail.nonEmpty)
              throw OperatorException("BinarizeOperator: more than one child expression.")
            else
              new BinarizeOperator(c.add(e, level - 1) :: child.tail)
          case _ =>
            throw OperatorException("BinarizeOperator: requires one argument")
        }
      case _ => throw OperatorException("BinarizeOperator: trying to add to smt else than an operator.")
    }
  }

  def evaluate(query: String, data: AnalyzersDataInternal = AnalyzersDataInternal()): Result = {
    val res = child.headOption match {
      case Some(arg) => arg.matches(query, data)
      case _ => throw OperatorException("BinarizeOperator: inner expression is empty")
    }
    if (equiv(res.score, 0.0d))
      Result(
        score = 0.0d,
        data = data
      )
    else
      Result(
        score = 1.0d,
        AnalyzersDataInternal(
          context = data.context,
          stateVariables = StateVariables(
            traversedStates = data.stateVariables.traversedStates,
            extractedVariables = data.stateVariables.extractedVariables ++ res.data.stateVariables.extractedVariables
          ),
          data = data.data ++ res.data.data
        )
      )
  }
}
