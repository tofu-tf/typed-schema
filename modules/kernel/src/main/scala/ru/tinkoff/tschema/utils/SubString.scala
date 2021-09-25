package ru.tinkoff.tschema.utils

import ru.tinkoff.tschema.utils.SubString.outOfBound

import scala.annotation.tailrec
import scala.util.hashing.MurmurHash3

final class SubString private[SubString] (private val arr: Array[Char], private val from: Int, private val to: Int)
    extends CharSequence {
  @inline private[this] def size                      = to - from
  def length(): Int                                   = size
  def charAt(index: Int): Char                        = {
    if (index >= size || index < 0) outOfBound(index)
    arr(from + index)
  }
  def subSequence(start: Int, end: Int): CharSequence = {
    if (start < 0) outOfBound(start)
    if (end > size) outOfBound(end)
    val len = end - start
    if (len < 0) outOfBound(len)
    if ((start == 0) && (end == size)) this
    else new SubString(arr, from + start, from + end)
  }
  override def equals(obj: Any): Boolean              = obj match {
    case s: CharSequence =>
      @tailrec def go(i: Int): Boolean = (i == size) || ((s.charAt(i) == charAt(i)) && go(i + 1))
      size == s.length() && go(0)
    case _               => false
  }

  override def hashCode(): Int                        = {

    /** Compute the hash of a string */
    var h = MurmurHash3.stringSeed
    var i = from
    while (i + 1 < to) {
      val data = (arr(i) << 16) + arr(i + 1)
      h = MurmurHash3.mix(h, data)
      i += 2
    }
    if (i < to) h = MurmurHash3.mixLast(h, arr(i).toInt)
    MurmurHash3.finalizeHash(h, size)
  }

  override def toString: String = new String(arr.slice(from, to))

}

object SubString {
  @inline private def outOfBound(index: Int) = throw new StringIndexOutOfBoundsException(index)

  def apply(s: String): SubString = new SubString(s.toCharArray, 0, s.length)
}
