package ru.tinkoff.tschema

package object macros {
  type Skip[A] = Any

  type NList[T]      = List[(String, T)]
  type MethodDecl[T] = (List[NList[T]], T)
}
