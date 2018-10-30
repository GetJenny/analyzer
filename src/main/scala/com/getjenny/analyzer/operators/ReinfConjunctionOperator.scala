package com.getjenny.analyzer.operators

import com.getjenny.analyzer.expressions._
import scalaz.Scalaz._

/**
  * Created by angelo on 18/01/2018.
  */

class ReinfConjunctionOperator(children: List[Expression]) extends AbstractOperator(children: List[Expression]) {
  override def toString: String = "ReinfConjunctionOperator(" + children.mkString(", ") + ")"
  def add(e: Expression, level: Int = 0): AbstractOperator = {
    if (level === 0) {
      new ReinfConjunctionOperator(e :: children)
    } else if(children.isEmpty){
      throw OperatorException("ReinfConjunctionOperator children list is empty")
    } else {
      children.headOption match {
        case Some(t) =>
          t match {
            case c: AbstractOperator => new ReinfConjunctionOperator(c.add(e, level - 1) :: children.tail)
            case _ => throw OperatorException("ReinfConjunctionOperator: trying to add to smt else than an operator")
          }
        case _ =>
          throw OperatorException("ReinfConjunctionOperator: trying to add None instead of an operator")
      }
    }
  }

  def evaluate(query: String, data: AnalyzersDataInternal = new AnalyzersDataInternal): Result = {
    def reinfConjunction(l: List[Expression]): Result = {
      val res = l.head.evaluate(query, data)
      if (l.tail.isEmpty) {
//        println("SCORE_NIL: " + res.score * 1.1 + "(" + res.score + ")")
        Result(score = res.score * 1.1,
          AnalyzersDataInternal(
            traversed_states = data.traversed_states,
            extracted_variables = data.extracted_variables ++ res.data.extracted_variables,
            data = res.data.data
          )
        )
      } else {
        val val1 = l.head.evaluate(query, data)
        val val2 = reinfConjunction(l.tail)
//        println("SCORE_NOT_NIL: " + (val1.score * 1.1) * val2.score + "(" + val1.score + ")" + "(" + val2.score + ")")
        Result(score = (val1.score * 1.1) * val2.score,
          AnalyzersDataInternal(
            traversed_states = data.traversed_states,
            extracted_variables = val1.data.extracted_variables ++ val2.data.extracted_variables,
            data = data.data ++ val1.data.data ++ val2.data.data
          )
        )
      }
    }
    reinfConjunction(children)
  }
}
