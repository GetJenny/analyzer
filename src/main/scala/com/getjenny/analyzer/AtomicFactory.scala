package com.getjenny.analyzer

/**
  * Created by mal on 20/02/2017.
  */
object AtomicFactory {

  val atoms = Set("keyword" , "similar", "synonym", "regex")

  def returnAtoms(name: String, argument: String): Atomic = name match {

    case "keyword" => new Keyword(argument)
    case "similar" => new W2VCosineSentence(argument)
    case "synonym" => new W2VCosineWord(argument)
    case "regex" => new RegularExpression(argument)
//    case "or" => new or()
//    case "and" => new and()
//    case "conjunction" => new conjunction()
//    case "disjunction" => new disjunction()
    case _ => throw new Exception("Atom \'" + name + "\' not found")

  }

}
