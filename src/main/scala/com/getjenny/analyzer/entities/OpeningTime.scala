package com.getjenny.analyzer.entities

/**
  * Created by angelo on 13/02/2019.
  */

case class OpeningTime(
                        OpenTime: String, /** “HH:MM”, 24 h format */
                        CloseTime: String, /** “HH:MM”, 24 h format */
                        Timezone: String, /** any of UTC, GMT, UT, CET, UTC+<N>, UTC-<N>, GMT+<N> */
                        Months: Set[Int], /** e.g. {1,2,3}, // set of months 1 to 12 */
                        Days: Set[Int], /** e.g. {1,2,3}, // set of days 1 to 31 */
                        WeekDays: Set[Int] /** e.g. {1,2,3,4,5} // set of weekdays 1(Monday) - 7(Sunday) */
                      )
