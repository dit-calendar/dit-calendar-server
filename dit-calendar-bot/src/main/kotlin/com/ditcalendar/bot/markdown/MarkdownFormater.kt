package com.ditcalendar.bot.markdown

import com.ditcalendar.bot.config.bot_name
import com.ditcalendar.bot.config.config
import com.ditcalendar.bot.data.*


private val config by config()

private val botName = config[bot_name]

fun Task.toMarkdown(): String =
        when (this) {
            is TaskForAssignment ->
                """
                    _Task_: $description [assign me](https://t.me/$botName?start=assign_$taskId)
                    _Datum_: $startTime
                """.trimIndent()
            is TaskForUnassignment ->
                """
                    _Task_: $description
                    _Datum_: $startTime
                    [unassign me](https://t.me/$botName?start=unassign_$taskId)
                """.trimIndent()
            is TaskAfterUnassignment ->
                """
                    erfolgreich ausgetragen von:
                    _Task_: $description
                    _Datum_: $startTime
                """.trimIndent()
        }


fun Tasks.toMarkdown(): String = joinToString(separator = System.lineSeparator()) { it.toMarkdown() }

fun DitCalendar.toMarkdown(): String =
        """
            *$description*
            *Datum*: $startDate
            
        """.trimIndent() + tasks.toMarkdown()