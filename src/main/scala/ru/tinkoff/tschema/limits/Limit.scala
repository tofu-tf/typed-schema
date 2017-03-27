package ru.tinkoff.tschema.limits
import ru.tinkoff.tschema.typeDSL.DSLAtom

/**
  * signs that servive acces should be limited
  *
  * @tparam params tuple of parameter names
  * @tparam rate   rate description
  */
final class Limit[params, rate <: Rate[_, _]] extends DSLAtom

final class Rate[count <: Int, unit <: TimeUnit]


