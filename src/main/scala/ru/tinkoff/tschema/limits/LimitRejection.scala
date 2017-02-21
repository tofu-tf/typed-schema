package ru.tinkoff.tschema.limits
import akka.http.scaladsl.server.{Rejection, Route}
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.model.StatusCodes._


case class LimitRejection(key: String, count: Long, periodName: String, params: Seq[String]) extends Rejection

object LimitRejection{
  def handler: PartialFunction[Rejection, Route] = {
    case LimitRejection(key, count, periodName, Seq()) ⇒
      complete((TooManyRequests, s"for service $key only $count requests permitted per $periodName "))
    case LimitRejection(key, count, periodName, names) ⇒
      val nameDescr = names.mkString(",")
      complete((TooManyRequests, s"for service $key only $count requests permitted per $periodName for unique values of $nameDescr"))
  }
}
