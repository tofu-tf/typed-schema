package ru.tinkoff.tschema.limits
import akka.http.scaladsl.server.Rejection

case class LimitRejection(key: String, count: Long, periodName: String) extends Rejection
