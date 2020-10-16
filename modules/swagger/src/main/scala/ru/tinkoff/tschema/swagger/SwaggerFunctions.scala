package ru.tinkoff.tschema.swagger

import cats.Eval

trait SwaggerFunctions {
  import SwaggerTypeable.make

  def typeOf[A](implicit swagger: Swagger[A]): SwaggerType = swagger.typ

  def obj[A](props: (String, SwaggerType)*): Swagger[A] = make(
    SwaggerObject(
      props.map { case (name, typ) => SwaggerProperty(name, None, typ = Eval.now(typ)) }.toVector
    )
  )

  def objLazy[A](props: (String, () => SwaggerType)*): Swagger[A] = make(
    SwaggerObject(
      props.map { case (name, typ) => SwaggerProperty(name, None, typ = Eval.later(typ())) }.toVector
    )
  )

  def arr[A](t: => SwaggerType): Swagger[A] =
    SwaggerTypeable.make(SwaggerArray(Eval.later(t)))

  def oneOf[A](ts: SwaggerType*): Swagger[A] = make(
    SwaggerOneOf(ts.map(t => None -> Eval.now(t)).toVector)
  )

  def oneOfLazy[A](ts: () => SwaggerType*): Swagger[A] = make(
    SwaggerOneOf(ts.map(t => None -> Eval.later(t())).toVector)
  )

  def discriminated[A](discriminator: String)(ts: (String, SwaggerType)*): Swagger[A] = make(
    SwaggerOneOf(ts.map { case (name, t) => Some(name) -> Eval.now(t) }.toVector)
  )

  def discriminatedLazy[A](discriminator: String)(ts: (String, () => SwaggerType)*): Swagger[A] =
    SwaggerTypeable.make(
      SwaggerOneOf(ts.map { case (name, t) => Some(name) -> Eval.later(t()) }.toVector)
    )

  //usage : Swagger.string(_(format = OpenApiFormat.password, maxLength = 30))
  def string[A](create: SwaggerStringValue.type => SwaggerStringValue): Swagger[A] =
    SwaggerTypeable.make(
      new SwaggerPrimitive(create(SwaggerStringValue))
    )

  //usage : Swagger.int(_(format = OpenApiFormat.int64, minimum = 1))
  def int[A](create: SwaggerIntValue.type => SwaggerIntValue): Swagger[A] =
    SwaggerTypeable.make(
      new SwaggerPrimitive(create(SwaggerIntValue))
    )

  def number[A](create: SwaggerNumberValue.type => SwaggerNumberValue): Swagger[A] =
    SwaggerTypeable.make(
      new SwaggerPrimitive(create(SwaggerNumberValue))
    )
}
