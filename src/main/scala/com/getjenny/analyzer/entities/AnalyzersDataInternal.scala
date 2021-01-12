package com.getjenny.analyzer.entities

case class StateVariables(
                           traversedStates: Vector[DtHistoryItem] = Vector.empty[DtHistoryItem],
                           variables: Map[String, String] = Map.empty[String, String]
                        )

case class Context(
                    indexName: String = "",
                    stateName: String = ""
                  )

case class AnalyzersDataInternal(
                                  context: Context = Context(),
                                  stateData: StateVariables = StateVariables(),
                                  data: Map[String, Any] = Map.empty[String, Any]
                                )
