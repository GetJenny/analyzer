package com.getjenny.analyzer.operators

import com.getjenny.analyzer.entities.{AnalyzersDataInternal, Result, StateVariables}
import com.getjenny.analyzer.expressions._
import scalaz.Scalaz._

import scala.math.Ordering.Double.equiv

/**
  * Created by mal on 21/02/2017.
  */

class DisjunctionOperator(children: List[Expression]) extends AbstractOperator(children: List[Expression]) {
  override def toString: String = "DisjunctionOperator(" + children.mkString(", ") + ")"
  def add(e: Expression, level: Int = 0): AbstractOperator = {
    if (level === 0) new DisjunctionOperator(e :: children)
    else {
      children.headOption match {
        case Some(t) =>
          t match {
            case c: AbstractOperator => new DisjunctionOperator(c.add(e, level - 1) :: children.tail)
            case _ => throw OperatorException("DisjunctionOperator: trying to add to smt else than an operator")
          }
        case _ =>
          throw OperatorException("DisjunctionOperator: trying to add None instead of an operator")
      }
    }
  }

  def evaluate(query: String, data: AnalyzersDataInternal = new AnalyzersDataInternal): Result = {
    def compDisjunction(l: List[Expression]): Result = {
      val res = l.headOption match {
        case Some(arg) => arg.evaluate(query, data)
        case _ => throw OperatorException("DisjunctionOperator: inner expression is empty")
      }
      if (l.tail.isEmpty) {
        Result(score = 1.0d - res.score,
          AnalyzersDataInternal(
            context = data.context,
            stateVariables = StateVariables(
              traversedStates = data.stateVariables.traversedStates,
              extractedVariables = data.stateVariables.extractedVariables ++ res.data.stateVariables.extractedVariables
            ),
            data = data.data ++ res.data.data
          )
        )
      } else {
        val resTail = compDisjunction(l.tail)
        Result(score = (1.0d - res.score) * resTail.score,
          AnalyzersDataInternal(
            context = data.context,
            stateVariables = StateVariables(
              traversedStates = data.stateVariables.traversedStates,
              // map summation order is important, as res elements must override resTail existing elements
              extractedVariables = resTail.data.stateVariables.extractedVariables ++ res.data.stateVariables.extractedVariables
            ),
            data = resTail.data.data ++ res.data.data
          )
        )
      }
    }
    val resCompDisj = compDisjunction(children)
    val finalScore = 1.0d - resCompDisj.score
    if (equiv(finalScore, 0.0d)) {
      Result(
        score = finalScore,
        data = data
      )
    } else {
      Result(
        score = finalScore,
        data = resCompDisj.data
      )
    }
  }
}
