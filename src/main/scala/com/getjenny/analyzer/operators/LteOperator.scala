package com.getjenny.analyzer.operators

import com.getjenny.analyzer.expressions._
import scalaz._
import Scalaz._
import com.getjenny.analyzer.entities.{AnalyzersDataInternal, StateVariables, Result}

/** Compare Operator
  *
  * It compare the result of two Expressions
  *
  * Created by Angelo Leto on 19/10/2018.
  */

class LteOperator(child: List[Expression]) extends AbstractOperator(child: List[Expression]) {
  require(child.length <= 2, "LteOperator can only have two Expressions/Atoms as arguments")
  override def toString: String = "LteOperator(" + child.mkString(", ") + ")"

  def add(e: Expression, level: Int = 0): AbstractOperator = {
    if (level === 0) new LteOperator(e :: child)
    else if (level === 1) {
      child.headOption match {
        case Some(t) =>
          t match {
            case c: AbstractOperator => new LteOperator(c.add(e, level - 1) :: child.tail)
            case _ => throw OperatorException("LteOperator: trying to add to smt else than an operator")
          }
        case _ =>
          throw OperatorException("LteOperator: trying to add None instead of an operator")
      }
    } else {
      throw OperatorException("LteOperator: trying to add more than two expression")
    }
  }

  def evaluate(query: String, data: AnalyzersDataInternal = new AnalyzersDataInternal): Result = {
    val secondArgument = child.headOption match {
      case Some(t) => t
      case _ =>
        throw OperatorException("LteOperator: requires an expression as first argument")
    }

    val firstArgument = child.tail.headOption match {
      case Some(t) => t
      case _ =>
        throw OperatorException("LteOperator: requires an expression as second argument")
    }

    val res1: Result = firstArgument.evaluate(query = query, data = data)
    val res2: Result = secondArgument.evaluate(query = query, data = data)
    val score = if(res1.score <= res2.score) 1.0 else 0.0
    val resData = AnalyzersDataInternal(
      context = res1.data.context,
      stateVariables = StateVariables(
        traversedStates = res1.data.stateVariables.traversedStates,
        extractedVariables = res1.data.stateVariables.extractedVariables ++ res2.data.stateVariables.extractedVariables
      ),
      data = res1.data.data ++ res2.data.data
    )
    Result(score=score, data = resData)
  }
}
