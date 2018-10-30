package com.getjenny.analyzer.atoms

/**
  * Created by mal on 20/02/2017.
  */

import com.getjenny.analyzer.interfaces._

class DefaultFactoryAtomic extends AtomicFactoryTrait[List[String], AbstractAtomic, Map[String, String]] {

  override val operations = Set("keyword",
    "similar",
    "synonym",
    "regex",
    "matchPatternRegex",
    "matchDateDDMMYYYY",
    "existsVariable",
    "cosDistanceKeywords",
    "distance",
    "checkTimestamp",
    "checkDayOfWeek",
    "checkDayOfMonth",
    "checkMonth",
    "checkHour",
    "checkMinute",
    "double"
  )

  override def get(name: String, argument: List[String], restricted_args: Map[String, String]):
  AbstractAtomic = name.filter(c => !c.isWhitespace ) match {
    case "keyword" => new KeywordAtomic(argument, restricted_args)
    case "regex" => new RegularExpressionAtomic(argument, restricted_args)
    case "matchPatternRegex" => new MatchPatternRegexAtomic(argument, restricted_args)
    case "matchDateDDMMYYYY" => new MatchDateDDMMYYYYAtomic(argument, restricted_args)
    case "existsVariable" => new ExistsVariableAtomic(argument, restricted_args)
    case ("distance" | "cosDistanceKeywords") => new CosineDistanceAnalyzer(argument, restricted_args)
    case "checkTimestamp" => new CheckTimestampAtomic(argument, restricted_args)
    case "checkDayOfWeek" => new CheckDayOfWeekAtomic(argument, restricted_args)
    case "checkDayOfMonth" => new CheckDayOfMonthAtomic(argument, restricted_args)
    case "checkMonth" => new CheckMonthAtomic(argument, restricted_args)
    case "checkHour" => new CheckHourAtomic(argument, restricted_args)
    case "checkMinute" => new CheckMinuteAtomic(argument, restricted_args)
    case "double" => new DoubleNumberAtomic(argument, restricted_args)
    case _ => throw ExceptionAtomic("Atom \'" + name + "\' not found")
  }
}
