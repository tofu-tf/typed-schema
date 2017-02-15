package ru.tinkoff.tschema.named

trait ChooseTag[A, B] {
  type Out
}

class ChooseTagL2{
  type Aux[A, B, T] = ChooseTag[A, B] {type Out = T}

  implicit def bothDefined[A, B]: Aux[A, B, B] = aux

  protected def aux[A, B, T]: Aux[A, B, T] = inst.asInstanceOf[Aux[A, B, T]]
  private val inst = new ChooseTag[Any, Any] {}
}

class ChooseTagL1 extends ChooseTagL2{
  implicit def leftDefined[A]: Aux[A, Nothing, A] = aux

  implicit def rightDefined[A]: Aux[Nothing, A, A] = aux
}

object ChooseTag extends ChooseTagL1{
  implicit val chooseNothing: Aux[Nothing, Nothing, Nothing] = aux
}
