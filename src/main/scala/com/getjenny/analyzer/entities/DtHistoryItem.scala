package com.getjenny.analyzer.entities

/**
  * Created by Angelo Leto <angelo@getjenny.com> on 15/11/19.
  */

import scalaz.Scalaz._

object DtHistoryType extends Enumeration {
  val INTERNAL,
  EXTERNAL = DtHistoryType.Value
  def value(`type`: String): DtHistoryType.Value = values.find(_.toString === `type`).getOrElse(EXTERNAL)
}

case class DtHistoryItem(
                          state: String,
                          `type`: DtHistoryType.Value,
                          timestamp: Long = System.currentTimeMillis()
                        )
