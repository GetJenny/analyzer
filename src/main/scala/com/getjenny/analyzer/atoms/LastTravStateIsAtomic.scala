package com.getjenny.analyzer.atoms

import com.getjenny.analyzer.expressions.{AnalyzersDataInternal, Result}

/**
  * Created by angelo on 16/08/17.
  */

/** test the value of the last traversed state
  *
  * @param arguments of the state to be checked
  */

class LastTravStateIsAtomic(val arguments: List[String], restrictedArgs: Map[String, String]) extends AbstractAtomic {
  val name: String = arguments.headOption match {
    case Some(t) => t
    case _ =>
      throw ExceptionAtomic("lastTravStateIs requires an argument")
  }

  override def toString: String = "lastTravStateIs"
  val isEvaluateNormalized: Boolean = true

  /** Check if the last state into data.traversed_states is <state>
    *
    * @param query the user query
    * @param data the data
    * @return Result with 1.0 if the last state is <name> score = 0.0 otherwise
    */
  def evaluate(query: String, data: AnalyzersDataInternal = AnalyzersDataInternal()): Result = {
    data.traversedStates.lastOption match {
      case Some(t) =>
        t match {
          case `name` => Result(score = 1.0)
          case _ => Result(score = 0.0)
        }
      case _ => Result(score = 0.0)
    }
  }
}
