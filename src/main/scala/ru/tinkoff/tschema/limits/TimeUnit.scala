package ru.tinkoff.tschema.limits

sealed abstract class TimeUnit(val millis: Long)

case object milli extends TimeUnit(1)
case object second extends TimeUnit(1000 * milli.millis)
case object minute extends TimeUnit(60 * second.millis)
case object hour extends TimeUnit(60 * minute.millis)
case object day extends TimeUnit(24 * hour.millis)
case object month extends TimeUnit(year.millis / 12)
case object year extends TimeUnit((365.2425 * day.millis).toLong)



