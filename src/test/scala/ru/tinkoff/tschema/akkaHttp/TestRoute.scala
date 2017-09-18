package ru.tinkoff.tschema.akkaHttp
import java.math.BigInteger

import akka.http.scaladsl.server.Route
import ru.tinkoff.tschema.syntax._
import ru.tinkoff.tschema.typeDSL._
import akka.actor.ActorSystem
import shapeless._
import akka.http.scaladsl.client.RequestBuilding._
import akka.stream.ActorMaterializer
import ru.tinkoff.TestModule
import ru.tinkoff.definitions.StatsRes
import de.heikoseeberger.akkahttpcirce.FailFastCirceSupport._

import scala.concurrent.{Await, Future}
import scala.concurrent.duration._
import MkRoute.macroInterface.{RoutableOps, ServeOps}

object TestRoute {
  implicit val system = ActorSystem()
  import system.dispatcher
  implicit val materializer = ActorMaterializer()
  val uuu = queryParam[String]('uuu) :> operation('stats) :> ReqBody[Vector[BigDecimal]] :> get[StatsRes]

  type uuu = QueryParam[Witness.`'uuu`.T, String]
  type zzz = QueryParam[Witness.`'zzz`.T, Int]
  type vvv = Prefix[Witness.`'vvv`.T]

  def describe(s: String, i: Int): Future[String] = if (i == 0) Future("end!!")
  else for {
    next <- describe(s, i - 1)
  } yield s"$i: $s\n" + next

//  import akka.http.scaladsl.server.Directives._
//  val route: Route = (HNil: HNil).serve[uuu].apply { l =>
//    l.serve[vvv].apply { l1 =>
//      l1.serve[zzz].apply {
//        case i :: s :: _ =>
//          val res = Result[Get[String]]
//          describe(s, i).route[res.Out]
//      }
//    }
//  }

  class Something {
    def hello = "ururu"
  }

  def main(args: Array[String]): Unit = {




    val req = Get("/stats?uuu", content = Vector(BigDecimal(1), BigDecimal(2)))

    import TestModule.handler


    val route = MkRoute(uuu)(handler)
    val hndl = Route.asyncHandler(route)
    println(Await.result(hndl(req).flatMap(_.entity.toStrict(1.second)).map(_.getData().utf8String), 1 second))
    import shapeless._
    import shapeless.syntax.singleton._
    val inp = 'xxx ->> 2 :: 'body ->> Vector(BigDecimal(2), BigDecimal(3)) :: 2 ->> 2 :: HNil

  }
}
