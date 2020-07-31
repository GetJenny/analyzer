package com.getjenny.analyzer.analyzers

/**
  * Created by Michele Boggia <michele.boggia@getjenny.com> on 30/07/20.
  */

import com.getjenny.analyzer.expressions.AnalyzersDataInternal
import org.scalatest._
import org.scalatest.matchers.should.Matchers

class OperatorOrderTest extends FlatSpec with Matchers {

  val restrictedArgs: Map[String, String] = Map.empty[String, String]
  val travStateId: String = "00dac6ee-80a1-4054-baac-747a80bb7738"
  val emailExtracted: String = "test1@example.com"
  val emailInQuery: String = "test2@example.com"
  val queryWithEmail: String = "my email is " + emailInQuery
  val queryNoEmail: String = "this query does not contain any email address!"
  val extractedVarKey = "aaa"
  val extractedVarValue = "bbb"
  val extractedEmailKey = "email_address.0"
  val extractedQueryKey = "customer_message.0"
  val data: AnalyzersDataInternal = AnalyzersDataInternal(
    traversedStates = Vector[String](travStateId),
    extractedVariables = Map[String, String](extractedVarKey -> extractedVarValue, extractedEmailKey -> emailExtracted)
  )
  val atomExtractEmail: String = """matchPatternRegex("[email_address](?:([a-zA-Z0-9_.+-]+@[a-zA-Z0-9-]+.[a-zA-Z0-9-.]+))")"""
  val atomExtractQuery: String = """matchPatternRegex("[customer_message](.*)")"""
  val atomLastTravStateTrue: String = """lastTravStateIs("""" + travStateId + """")"""
  val atomLastTravStateFalse: String = """lastTravStateIs("1234")"""
  def analyzerString(operator: String, operands: List[String]): String = operator + "(" + operands.mkString(",") + ")"
  val scoreSuccess: Double = 1.0d
  val scoreFailure: Double = 0.0d
  val scoreSuccessReinf1: Double = 1.1d
  val scoreSuccessReinf2: Double = scoreSuccessReinf1 * scoreSuccessReinf1
  val scoreSuccessReinf3: Double = scoreSuccessReinf2 * scoreSuccessReinf1

  val operatorBooleanAnd: String = "booleanAnd"
  val operatorBooleanOr: String = "booleanOr"
  val operatorBooleanNot: String = "booleanNot"
  val operatorConjunction: String = "conjunction"
  val operatorDisjunction: String = "disjunction"
  val operatorReinfConjunction: String = "reinfConjunction"

  operatorBooleanAnd should
  "trigger and extract email address when matching regex pattern" in {
      val analyzer = new DefaultAnalyzer(
        analyzerString(operatorBooleanAnd, List(atomExtractEmail)),
        restrictedArgs
      )
      val analyzerValue = analyzer.evaluate(queryWithEmail, data)
      analyzerValue.score shouldBe scoreSuccess
      analyzerValue.data.extractedVariables(extractedVarKey) shouldBe extractedVarValue
      analyzerValue.data.extractedVariables(extractedEmailKey) shouldBe emailInQuery
      analyzerValue.data.traversedStates shouldBe Vector[String](travStateId)
    }
  it should "trigger and extract email address when matching regex pattern and lastTravStateIs is true" in {
    val analyzer = new DefaultAnalyzer(
      analyzerString(operatorBooleanAnd,List(atomExtractEmail, atomLastTravStateTrue)),
      restrictedArgs
    )
    val analyzerValue = analyzer.evaluate(queryWithEmail, data)
    analyzerValue.score shouldBe scoreSuccess
    analyzerValue.data.extractedVariables(extractedVarKey) shouldBe extractedVarValue
    analyzerValue.data.extractedVariables(extractedEmailKey) shouldBe emailInQuery
    analyzerValue.data.traversedStates shouldBe Vector[String](travStateId)
  }
  it should "trigger and extract email address when lastTravStateIs is true and matching regex pattern" in {
    val analyzer = new DefaultAnalyzer(
      analyzerString(operatorBooleanAnd,List(atomLastTravStateTrue, atomExtractEmail)),
      restrictedArgs
    )
    val analyzerValue = analyzer.evaluate(queryWithEmail, data)
    analyzerValue.score shouldBe scoreSuccess
    analyzerValue.data.extractedVariables(extractedVarKey) shouldBe extractedVarValue
    analyzerValue.data.extractedVariables(extractedEmailKey) shouldBe emailInQuery
    analyzerValue.data.traversedStates shouldBe Vector[String](travStateId)
  }
  it should "not trigger and not update email address when matching regex pattern and lastTravStateIs is false" in {
    val analyzer = new DefaultAnalyzer(
      analyzerString(operatorBooleanAnd,List(atomExtractEmail, atomLastTravStateFalse)),
      restrictedArgs
    )
    val analyzerValue = analyzer.evaluate(queryWithEmail, data)
    analyzerValue.score shouldBe scoreFailure
    analyzerValue.data.extractedVariables(extractedVarKey) shouldBe extractedVarValue
    analyzerValue.data.extractedVariables(extractedEmailKey) shouldBe emailExtracted
    analyzerValue.data.traversedStates shouldBe Vector[String](travStateId)
  }
  it should "not trigger and not update email address when lastTravStateIs is false and matching regex pattern" in {
    val analyzer = new DefaultAnalyzer(
      analyzerString(operatorBooleanAnd,List(atomLastTravStateFalse, atomExtractEmail)),
      restrictedArgs
    )
    val analyzerValue = analyzer.evaluate(queryWithEmail, data)
    analyzerValue.score shouldBe scoreFailure
    analyzerValue.data.extractedVariables(extractedVarKey) shouldBe extractedVarValue
    analyzerValue.data.extractedVariables(extractedEmailKey) shouldBe emailExtracted
    analyzerValue.data.traversedStates shouldBe Vector[String](travStateId)
  }
  it should "trigger and extract both email address and customer message" in {
    val analyzer = new DefaultAnalyzer(
      analyzerString(operatorBooleanAnd,List(atomLastTravStateTrue, atomExtractEmail, atomExtractQuery)),
      restrictedArgs
    )
    val analyzerValue = analyzer.evaluate(queryWithEmail, data)
    analyzerValue.score shouldBe scoreSuccess
    analyzerValue.data.extractedVariables(extractedVarKey) shouldBe extractedVarValue
    analyzerValue.data.extractedVariables(extractedEmailKey) shouldBe emailInQuery
    analyzerValue.data.extractedVariables(extractedQueryKey) shouldBe queryWithEmail
    analyzerValue.data.traversedStates shouldBe Vector[String](travStateId)
  }

  operatorBooleanOr should
    "trigger and extract email address when matching regex pattern" in {
    val analyzer = new DefaultAnalyzer(
      analyzerString(operatorBooleanOr, List(atomExtractEmail)),
      restrictedArgs
    )
    val analyzerValue = analyzer.evaluate(queryWithEmail, data)
    analyzerValue.score shouldBe scoreSuccess
    analyzerValue.data.extractedVariables(extractedVarKey) shouldBe extractedVarValue
    analyzerValue.data.extractedVariables(extractedEmailKey) shouldBe emailInQuery
    analyzerValue.data.traversedStates shouldBe Vector[String](travStateId)
  }
  it should "trigger and extract email address when matching regex pattern and lastTravStateIs is false" in {
    val analyzer = new DefaultAnalyzer(
      analyzerString(operatorBooleanOr,List(atomExtractEmail, atomLastTravStateFalse)),
      restrictedArgs
    )
    val analyzerValue = analyzer.evaluate(queryWithEmail, data)
    analyzerValue.score shouldBe scoreSuccess
    analyzerValue.data.extractedVariables(extractedVarKey) shouldBe extractedVarValue
    analyzerValue.data.extractedVariables(extractedEmailKey) shouldBe emailInQuery
    analyzerValue.data.traversedStates shouldBe Vector[String](travStateId)
  }
  it should "trigger and extract email address when lastTravStateIs is false and matching regex pattern" in {
    val analyzer = new DefaultAnalyzer(
      analyzerString(operatorBooleanOr,List(atomLastTravStateFalse, atomExtractEmail)),
      restrictedArgs
    )
    val analyzerValue = analyzer.evaluate(queryWithEmail, data)
    analyzerValue.score shouldBe scoreSuccess
    analyzerValue.data.extractedVariables(extractedVarKey) shouldBe extractedVarValue
    analyzerValue.data.extractedVariables(extractedEmailKey) shouldBe emailInQuery
    analyzerValue.data.traversedStates shouldBe Vector[String](travStateId)
  }
  it should "trigger and extract both email address and customer message" in {
    val analyzer = new DefaultAnalyzer(
      analyzerString(operatorBooleanOr,List(atomLastTravStateFalse, atomExtractEmail, atomExtractQuery)),
      restrictedArgs
    )
    val analyzerValue = analyzer.evaluate(queryWithEmail, data)
    analyzerValue.score shouldBe scoreSuccess
    analyzerValue.data.extractedVariables(extractedVarKey) shouldBe extractedVarValue
    analyzerValue.data.extractedVariables(extractedEmailKey) shouldBe emailInQuery
    analyzerValue.data.extractedVariables(extractedQueryKey) shouldBe queryWithEmail
    analyzerValue.data.traversedStates shouldBe Vector[String](travStateId)
  }

  operatorBooleanNot should
    "trigger and return previously extracted email when not matching regex pattern" in {
    val analyzer = new DefaultAnalyzer(
      analyzerString(operatorBooleanNot, List(atomExtractEmail)),
      restrictedArgs
    )
    val analyzerValue = analyzer.evaluate(queryNoEmail, data)
    analyzerValue.score shouldBe scoreSuccess
    analyzerValue.data.extractedVariables(extractedVarKey) shouldBe extractedVarValue
    analyzerValue.data.extractedVariables(extractedEmailKey) shouldBe emailExtracted
    analyzerValue.data.traversedStates shouldBe Vector[String](travStateId)
  }
  it should "not trigger and return previously extracted email when matching regex pattern" in {
    val analyzer = new DefaultAnalyzer(
      analyzerString(operatorBooleanNot, List(atomExtractEmail)),
      restrictedArgs
    )
    val analyzerValue = analyzer.evaluate(queryWithEmail, data)
    analyzerValue.score shouldBe scoreFailure
    analyzerValue.data.extractedVariables(extractedVarKey) shouldBe extractedVarValue
    analyzerValue.data.extractedVariables(extractedEmailKey) shouldBe emailExtracted
    analyzerValue.data.traversedStates shouldBe Vector[String](travStateId)
  }

  operatorConjunction should
    "trigger and extract email address when matching regex pattern" in {
    val analyzer = new DefaultAnalyzer(
      analyzerString(operatorConjunction, List(atomExtractEmail)),
      restrictedArgs
    )
    val analyzerValue = analyzer.evaluate(queryWithEmail, data)
    analyzerValue.score shouldBe scoreSuccess
    analyzerValue.data.extractedVariables(extractedVarKey) shouldBe extractedVarValue
    analyzerValue.data.extractedVariables(extractedEmailKey) shouldBe emailInQuery
    analyzerValue.data.traversedStates shouldBe Vector[String](travStateId)
  }
  it should "trigger and extract email address when matching regex pattern and lastTravStateIs is true" in {
    val analyzer = new DefaultAnalyzer(
      analyzerString(operatorConjunction,List(atomExtractEmail, atomLastTravStateTrue)),
      restrictedArgs
    )
    val analyzerValue = analyzer.evaluate(queryWithEmail, data)
    analyzerValue.score shouldBe scoreSuccess
    analyzerValue.data.extractedVariables(extractedVarKey) shouldBe extractedVarValue
    analyzerValue.data.extractedVariables(extractedEmailKey) shouldBe emailInQuery
    analyzerValue.data.traversedStates shouldBe Vector[String](travStateId)
  }
  it should "trigger and extract email address when lastTravStateIs is true and matching regex pattern" in {
    val analyzer = new DefaultAnalyzer(
      analyzerString(operatorConjunction,List(atomLastTravStateTrue, atomExtractEmail)),
      restrictedArgs
    )
    val analyzerValue = analyzer.evaluate(queryWithEmail, data)
    analyzerValue.score shouldBe scoreSuccess
    analyzerValue.data.extractedVariables(extractedVarKey) shouldBe extractedVarValue
    analyzerValue.data.extractedVariables(extractedEmailKey) shouldBe emailInQuery
    analyzerValue.data.traversedStates shouldBe Vector[String](travStateId)
  }
  it should "not trigger and not update email address when matching regex pattern and lastTravStateIs is false" in {
    val analyzer = new DefaultAnalyzer(
      analyzerString(operatorConjunction,List(atomExtractEmail, atomLastTravStateFalse)),
      restrictedArgs
    )
    val analyzerValue = analyzer.evaluate(queryWithEmail, data)
    analyzerValue.score shouldBe scoreFailure
    analyzerValue.data.extractedVariables(extractedVarKey) shouldBe extractedVarValue
    analyzerValue.data.extractedVariables(extractedEmailKey) shouldBe emailExtracted
    analyzerValue.data.traversedStates shouldBe Vector[String](travStateId)
  }
  it should "not trigger and not update email address when lastTravStateIs is false and matching regex pattern" in {
    val analyzer = new DefaultAnalyzer(
      analyzerString(operatorConjunction,List(atomLastTravStateFalse, atomExtractEmail)),
      restrictedArgs
    )
    val analyzerValue = analyzer.evaluate(queryWithEmail, data)
    analyzerValue.score shouldBe scoreFailure
    analyzerValue.data.extractedVariables(extractedVarKey) shouldBe extractedVarValue
    analyzerValue.data.extractedVariables(extractedEmailKey) shouldBe emailExtracted
    analyzerValue.data.traversedStates shouldBe Vector[String](travStateId)
  }
  it should "trigger and extract both email address and customer message" in {
    val analyzer = new DefaultAnalyzer(
      analyzerString(operatorConjunction,List(atomLastTravStateTrue, atomExtractEmail, atomExtractQuery)),
      restrictedArgs
    )
    val analyzerValue = analyzer.evaluate(queryWithEmail, data)
    analyzerValue.score shouldBe scoreSuccess
    analyzerValue.data.extractedVariables(extractedVarKey) shouldBe extractedVarValue
    analyzerValue.data.extractedVariables(extractedEmailKey) shouldBe emailInQuery
    analyzerValue.data.extractedVariables(extractedQueryKey) shouldBe queryWithEmail
    analyzerValue.data.traversedStates shouldBe Vector[String](travStateId)
  }

  operatorDisjunction should
    "trigger and extract email address when matching regex pattern" in {
    val analyzer = new DefaultAnalyzer(
      analyzerString(operatorDisjunction, List(atomExtractEmail)),
      restrictedArgs
    )
    val analyzerValue = analyzer.evaluate(queryWithEmail, data)
    analyzerValue.score shouldBe scoreSuccess
    analyzerValue.data.extractedVariables(extractedVarKey) shouldBe extractedVarValue
    analyzerValue.data.extractedVariables(extractedEmailKey) shouldBe emailInQuery
    analyzerValue.data.traversedStates shouldBe Vector[String](travStateId)
  }
  it should "trigger and extract email address when matching regex pattern and lastTravStateIs is false" in {
    val analyzer = new DefaultAnalyzer(
      analyzerString(operatorDisjunction,List(atomExtractEmail, atomLastTravStateFalse)),
      restrictedArgs
    )
    val analyzerValue = analyzer.evaluate(queryWithEmail, data)
    analyzerValue.score shouldBe scoreSuccess
    analyzerValue.data.extractedVariables(extractedVarKey) shouldBe extractedVarValue
    analyzerValue.data.extractedVariables(extractedEmailKey) shouldBe emailInQuery
    analyzerValue.data.traversedStates shouldBe Vector[String](travStateId)
  }
  it should "trigger and extract email address when lastTravStateIs is false and matching regex pattern" in {
    val analyzer = new DefaultAnalyzer(
      analyzerString(operatorDisjunction,List(atomLastTravStateFalse, atomExtractEmail)),
      restrictedArgs
    )
    val analyzerValue = analyzer.evaluate(queryWithEmail, data)
    analyzerValue.score shouldBe scoreSuccess
    analyzerValue.data.extractedVariables(extractedVarKey) shouldBe extractedVarValue
    analyzerValue.data.extractedVariables(extractedEmailKey) shouldBe emailInQuery
    analyzerValue.data.traversedStates shouldBe Vector[String](travStateId)
  }
  it should "trigger and extract both email address and customer message" in {
    val analyzer = new DefaultAnalyzer(
      analyzerString(operatorDisjunction,List(atomLastTravStateFalse, atomExtractEmail, atomExtractQuery)),
      restrictedArgs
    )
    val analyzerValue = analyzer.evaluate(queryWithEmail, data)
    analyzerValue.score shouldBe scoreSuccess
    analyzerValue.data.extractedVariables(extractedVarKey) shouldBe extractedVarValue
    analyzerValue.data.extractedVariables(extractedEmailKey) shouldBe emailInQuery
    analyzerValue.data.extractedVariables(extractedQueryKey) shouldBe queryWithEmail
    analyzerValue.data.traversedStates shouldBe Vector[String](travStateId)
  }

  operatorReinfConjunction should
    "trigger and extract email address when matching regex pattern" in {
    val analyzer = new DefaultAnalyzer(
      analyzerString(operatorReinfConjunction, List(atomExtractEmail)),
      restrictedArgs
    )
    val analyzerValue = analyzer.evaluate(queryWithEmail, data)
    analyzerValue.score shouldBe scoreSuccessReinf1
    analyzerValue.data.extractedVariables(extractedVarKey) shouldBe extractedVarValue
    analyzerValue.data.extractedVariables(extractedEmailKey) shouldBe emailInQuery
    analyzerValue.data.traversedStates shouldBe Vector[String](travStateId)
  }
  it should "trigger and extract email address when matching regex pattern and lastTravStateIs is true" in {
    val analyzer = new DefaultAnalyzer(
      analyzerString(operatorReinfConjunction,List(atomExtractEmail, atomLastTravStateTrue)),
      restrictedArgs
    )
    val analyzerValue = analyzer.evaluate(queryWithEmail, data)
    analyzerValue.score shouldBe scoreSuccessReinf2
    analyzerValue.data.extractedVariables(extractedVarKey) shouldBe extractedVarValue
    analyzerValue.data.extractedVariables(extractedEmailKey) shouldBe emailInQuery
    analyzerValue.data.traversedStates shouldBe Vector[String](travStateId)
  }
  it should "trigger and extract email address when lastTravStateIs is true and matching regex pattern" in {
    val analyzer = new DefaultAnalyzer(
      analyzerString(operatorReinfConjunction,List(atomLastTravStateTrue, atomExtractEmail)),
      restrictedArgs
    )
    val analyzerValue = analyzer.evaluate(queryWithEmail, data)
    analyzerValue.score shouldBe scoreSuccessReinf2
    analyzerValue.data.extractedVariables(extractedVarKey) shouldBe extractedVarValue
    analyzerValue.data.extractedVariables(extractedEmailKey) shouldBe emailInQuery
    analyzerValue.data.traversedStates shouldBe Vector[String](travStateId)
  }
  it should "not trigger and not update email address when matching regex pattern and lastTravStateIs is false" in {
    val analyzer = new DefaultAnalyzer(
      analyzerString(operatorReinfConjunction,List(atomExtractEmail, atomLastTravStateFalse)),
      restrictedArgs
    )
    val analyzerValue = analyzer.evaluate(queryWithEmail, data)
    analyzerValue.score shouldBe scoreFailure
    analyzerValue.data.extractedVariables(extractedVarKey) shouldBe extractedVarValue
    analyzerValue.data.extractedVariables(extractedEmailKey) shouldBe emailExtracted
    analyzerValue.data.traversedStates shouldBe Vector[String](travStateId)
  }
  it should "not trigger and not update email address when lastTravStateIs is false and matching regex pattern" in {
    val analyzer = new DefaultAnalyzer(
      analyzerString(operatorReinfConjunction,List(atomLastTravStateFalse, atomExtractEmail)),
      restrictedArgs
    )
    val analyzerValue = analyzer.evaluate(queryWithEmail, data)
    analyzerValue.score shouldBe scoreFailure
    analyzerValue.data.extractedVariables(extractedVarKey) shouldBe extractedVarValue
    analyzerValue.data.extractedVariables(extractedEmailKey) shouldBe emailExtracted
    analyzerValue.data.traversedStates shouldBe Vector[String](travStateId)
  }
  it should "trigger and extract both email address and customer message" in {
    val analyzer = new DefaultAnalyzer(
      analyzerString(operatorReinfConjunction,List(atomLastTravStateTrue, atomExtractEmail, atomExtractQuery)),
      restrictedArgs
    )
    val analyzerValue = analyzer.evaluate(queryWithEmail, data)
    analyzerValue.score shouldBe scoreSuccessReinf3
    analyzerValue.data.extractedVariables(extractedVarKey) shouldBe extractedVarValue
    analyzerValue.data.extractedVariables(extractedEmailKey) shouldBe emailInQuery
    analyzerValue.data.extractedVariables(extractedQueryKey) shouldBe queryWithEmail
    analyzerValue.data.traversedStates shouldBe Vector[String](travStateId)
  }
}
