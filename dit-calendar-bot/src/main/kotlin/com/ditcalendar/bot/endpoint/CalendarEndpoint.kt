package com.ditcalendar.bot.endpoint

import com.ditcalendar.bot.config.config
import com.ditcalendar.bot.config.dit_calendar_server_url
import com.ditcalendar.bot.data.DitCalendar
import com.github.kittinunf.fuel.httpGet
import com.github.kittinunf.result.Result
import com.github.kittinunf.result.flatMap
import kotlinx.serialization.UnstableDefault
import kotlinx.serialization.json.Json
import kotlinx.serialization.json.JsonConfiguration

class CalendarEndpoint {

    private val config by config()


    @UnstableDefault
    fun readCalendar(calendarId: Long): Result<DitCalendar, Exception> {

        val ditCalendarUrl = config[dit_calendar_server_url]

        val (_, _, result) = "$ditCalendarUrl/calendarentries/$calendarId"
                .httpGet()
                .responseString()

        return result.flatMap {
            Result.of<DitCalendar, Exception> {
                Json(JsonConfiguration.Stable.copy(strictMode = false))
                        .parse(DitCalendar.serializer(), result.get())
            }
        }
    }
}