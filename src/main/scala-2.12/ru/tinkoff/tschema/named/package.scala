package ru.tinkoff.tschema

package named {
  case object body
}

package object named {
  type body = body.type
}
