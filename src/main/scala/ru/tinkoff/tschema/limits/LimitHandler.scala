package ru.tinkoff.tschema.limits
import ru.tinkoff.tschema.limits.LimitHandler.{LimitRate, Pat, Pattern}
import shapeless.HList

import scala.collection.concurrent.TrieMap
import scala.concurrent.duration.FiniteDuration
import scala.concurrent.{ExecutionContext, Future}

trait LimitHandler {
  def check(pattern: Pat): Future[LimitHandler.Result]
}

object LimitHandler {
  type Pat = Pattern[_]

  final case class Pattern[L <: HList](key: String, list: L)
  final case class LimitRate(count: Int, duration: FiniteDuration)

  sealed trait Result
  case object Success extends Result
  final case class Exceeded(rate: LimitRate) extends Result

  def trieMap(getRate: Pattern[_] => LimitRate)(implicit ec: ExecutionContext): LimitHandler = new TrieMapLimitHandler(getRate)
}

class TrieMapLimitHandler private[limits](getRate: Pattern[_] => LimitRate)(implicit ec: ExecutionContext) extends LimitHandler {
  case class Record(started: Long, count: Long)

  val counter = new TrieMap[Pat, Record]

  def check(pattern: Pat): Future[LimitHandler.Result] = Future {
    val now = System.currentTimeMillis()
    val newRecord = Record(now, 1)
    val rate = getRate(pattern)
    counter.getOrElseUpdate(pattern, newRecord) match {
      case req if newRecord eq req => LimitHandler.Success
      case Record(time, count) if time + rate.duration.toMillis < now =>
        counter.put(pattern, newRecord)
        LimitHandler.Success
      case Record(time, count) if count < rate.count =>
        counter.put(pattern, Record(time, count + 1))
        LimitHandler.Success
      case _ => LimitHandler.Exceeded(rate)
    }
  }
}
