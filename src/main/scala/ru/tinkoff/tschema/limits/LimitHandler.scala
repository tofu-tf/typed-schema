package ru.tinkoff.tschema.limits
import ru.tinkoff.tschema.limits.LimitHandler.{LimitRate, Pat, Pattern}
import shapeless.HList

import scala.collection.concurrent.TrieMap
import scala.concurrent.{ExecutionContext, Future}

trait LimitHandler {
  def check(pattern: Pat, rate: LimitRate): Future[Boolean]
}

object LimitHandler {
  type Pat = Pattern[_]

  final case class Pattern[L <: HList](key: String, list: L)
  final case class LimitRate(count: Int, periodMillis: Long)

  def trieMap(implicit ec: ExecutionContext): LimitHandler = new TrieMapLimitHandler
}

class TrieMapLimitHandler private[limits](implicit ec: ExecutionContext) extends LimitHandler {
  case class Record(started: Long, count: Long)

  val counter = new TrieMap[Pat, Record]

  def check(pattern: Pat, rate: LimitRate): Future[Boolean] = Future {
    val now = System.currentTimeMillis()
    val newRecord = Record(now, 1)
    counter.getOrElseUpdate(pattern, newRecord) match {
      case req if newRecord eq req ⇒ true
      case Record(time, count) if time + rate.periodMillis < now ⇒
        counter.put(pattern, newRecord)
        true
      case Record(time, count) if count < rate.count ⇒
        counter.put(pattern, Record(time, count + 1))
        true
      case _ ⇒ false
    }
  }
}
