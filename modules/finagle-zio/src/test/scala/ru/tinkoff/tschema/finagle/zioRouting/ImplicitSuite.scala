package ru.tinkoff.tschema.finagle.zioRouting

import ru.tinkoff.tschema.finagle.{LiftHttp, Routed, RoutedPlus}
import zio.{Has, UIO, URIO, ZIO}
import zio.blocking.Blocking
import zio.clock.Clock

case class TestEnv()

class ImplicitSuite {
  type FullEnv       = Has[TestEnv] with Blocking with Clock
  type Test[+A]      = URIO[FullEnv, A]
  type Test2[+A]     = ZIO[FullEnv, String, A]
  type TestH[+A]     = URIOH[FullEnv, A]
  type Test2H[+A]    = ZIOH[FullEnv, String, A]
  type TestHttp[+A]  = URIOHttp[FullEnv, A]
  type Test2Http[+A] = ZIOHttp[FullEnv, String, A]

  implicitly[LiftHttp[TestHttp, Test]]
  implicitly[LiftHttp[TestH, Test]]
  implicitly[LiftHttp[UIOHttp, UIO]]

  implicitly[RoutedPlus[TestHttp]]
  implicitly[RoutedPlus[Test2Http]]
  implicitly[RoutedPlus[TestH]]
  implicitly[RoutedPlus[Test2H]]
  implicitly[RoutedPlus[UIOHttp]]
  implicitly[RoutedPlus[UIOH]]
}
