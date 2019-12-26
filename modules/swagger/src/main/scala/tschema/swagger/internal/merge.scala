package tschema.swagger.internal

object merge {
  def mergeOptWith[A](xo: Option[A], yo: Option[A])(f: (A, A) => A): Option[A] =
    xo match {
      case None => yo
      case Some(x) => yo match {
        case None => xo
        case Some(y) => Some(f(x, y))
      }
    }
}
