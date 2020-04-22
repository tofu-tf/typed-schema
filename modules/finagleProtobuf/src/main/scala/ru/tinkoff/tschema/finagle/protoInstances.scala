package ru.tinkoff.tschema.finagle

import ru.tinkoff.tschema.syntax.{Maker, MkComplex}

object protoInstances {
  def protoBody[x] =
    new MkComplex(new Maker[x, ProtoBody] {
      override def make[name]: ProtoBody[name, x] = new ProtoBody
    })
}
