package com.getjenny.analyzer.expressions

case class AnalyzersData(
                          traversedStates: Vector[String] = Vector.empty[String],
                          extractedVariables: Map[String, String] = Map.empty[String, String]
               )

case class AnalyzersDataInternal(
                                  traversedStates: Vector[String] = Vector.empty[String],
                                  extractedVariables: Map[String, String] = Map.empty[String, String],
                                  data: Map[String, Any] = Map.empty[String, Any]
                        )
