package ru.tinkoff.tschema.utils

object transform {
  private[this] val FirstPart     = "([A-Z]+)([A-Z][a-z])"
  private[this] val FollowingPart = "([a-z\\d])([A-Z])"
  private[this] val replaceSnake  = "$1_$2"
  private[this] val replaceKebab  = "$1-$2"

  private[this] def changeCase(s: String, rep: String): String = s
    .replaceAll(FirstPart, rep)
    .replaceAll(FollowingPart, rep)
    .toLowerCase

  def snakeCase(s: String): String = changeCase(s, replaceSnake)

  def kebabCase(s: String): String = changeCase(s, replaceKebab)
}
