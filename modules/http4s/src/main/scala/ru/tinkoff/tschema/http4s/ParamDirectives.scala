package ru.tinkoff.tschema.http4s

import cats.data.OptionT
import cats.data.OptionT.none
import cats.effect.Sync
import cats.{ Applicative, Defer }
import ru.tinkoff.tschema.http4s.Routing.Routes
import ru.tinkoff.tschema.param.{ Param, ParamSource }
import shapeless._
import shapeless.labelled.{ FieldType, field }

trait ParamDirectives[S <: ParamSource] {
  def source: S
  def getByName[F[_] : Sync](name: String, f: Option[String] => Routes[F]): Routes[F]

  def getAllByName[F[_] : Sync](name: String, f: List[String] => Routes[F]): Routes[F] =
    getByName(name, opt => f(opt.toList))

  def direct[F[_] : Defer : Applicative, A, name, In <: HList](
    result: Param.Result[A],
    in: In,
    k: (FieldType[name, A] :: In) => Routes[F]
  ): Routes[F] = Routing[F] { req => result.fold(_ => none, a => k(field[name](a) :: in)(req)) }
}

abstract class ParamDirectivesSimple[S <: ParamSource](val source: S) extends ParamDirectives[S] {
  def getFromRequest[F[_]](name: String)(req: Routed[F]): List[String]

  def getByName[F[_] : Sync](name: String, f: Option[String] => Routes[F]): Routes[F] =
    Routing[F] { req => f(getFromRequest(name)(req).headOption)(req) }

  override def getAllByName[F[_] : Sync](name: String, f: List[String] => Routes[F]): Routes[F] =
    Routing[F] { req => f(getFromRequest(name)(req))(req) }
}

object ParamDirectives {
  def apply[S <: ParamSource](implicit dir: ParamDirectives[S]): ParamDirectives[S] = dir

  type TC[A <: ParamSource]  = ParamDirectives[A]
  type TCS[A <: ParamSource] = ParamDirectivesSimple[A]
  import ParamSource._

  implicit val queryParamDirective: TC[Query] = new TCS[Query](Query) {
    def getFromRequest[F[_]](name: String)(req: Routed[F]): List[String] = req.queryMultiParams.getOrElse(name, Nil)
  }

  implicit val cookieParamDirectives: TC[Cookie] = new TCS[Cookie](Cookie) {
    def getFromRequest[F[_]](name: String)(req: Routed[F]): List[String] = req.cookies.get(name).toList
  }

  implicit val pathParamDirectives: TC[ParamSource.Path] = new TC[ParamSource.Path] {
    def source: ParamSource.Path = ParamSource.Path
    def getByName[F[_] : Sync](name: String, f: Option[String] => Routes[F]): Routes[F] =
      Routing[F] { req =>
        val (segment, routed) = req.segment()
        f(segment)(routed)
      }
  }

  implicit val formDataParamDirectives: TC[Form] = new TC[Form] {
    def source: Form = ParamSource.Form

    def getByName[F[_] : Sync](name: String, f: Option[String] => Routes[F]): Routes[F] =
      getAllByName(name, list => f(list.headOption))

    override def getAllByName[F[_] : Sync](name: String, f: List[String] => Routes[F]): Routes[F] =
      Routing[F] { req =>
        for {
          params <- OptionT.liftF(req.formParams)
          resp   <- f(params.getOrElse(name, Nil))(req)
        } yield resp
      }
  }

  implicit val headerParamDirectives: TC[Header] = new TCS[Header](Header) {
    def getFromRequest[F[_]](name: String)(req: Routed[F]): List[String] = req.headerMap.get(name).toList
  }
}
