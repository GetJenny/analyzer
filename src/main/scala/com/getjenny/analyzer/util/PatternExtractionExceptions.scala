package com.getjenny.analyzer.util

/**
  * Created by angelo on 26/06/17.
  */

case class PatternExtractionDeclarationParsingException(message: String = "", cause: Throwable = None.orNull)
  extends Exception(message, cause)

case class PatternExtractionParsingException(message: String = "", cause: Throwable = None.orNull)
  extends Exception(message, cause)

case class PatternExtractionNoMatchException(message: String = "", cause: Throwable = None.orNull)
  extends Exception(message, cause)

case class PatternExtractionBadSpecificationException(message: String = "", cause: Throwable = None.orNull)
  extends Exception(message, cause)
