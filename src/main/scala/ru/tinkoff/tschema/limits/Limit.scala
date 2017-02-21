package ru.tinkoff.tschema.limits

/**
  * signs that servive acces should be limited
  *
  * @tparam params tuple of parameter names
  * @tparam rate   rate description
  */
final class Limit[params, rate <: Rate[_, _]]



final class Rate[count <: Int, unit <: TimeUnit]


