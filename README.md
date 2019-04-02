# Typed Schema

[![maven central][maven badge]][maven search]

Typed schema is an http service definition DSL,
currently translating to [akka-http Routes]
and [OpenApi 3.0 definition][open-api-3.0] inspired by the [haskell-servant] library.

#### SBT
```SBT
libraryDependencies += "ru.tinkoff" %% "typed-schema" % "0.10.6"
```

## Main IDEA

### Motivation
We the People building services using modern scala often struggling to satisfy following requirements
* Service implementation should be checked to be compatible with OpenApi 3.0 specifications at the compile time
* Service definition should be detachable from the implementation and exportable as mere specification
* There should be an easy way to migrate all the services to different effect\future\task implementation
without changing any definition
* There should be some way to migrate all the service to another framework without reimplementing them

## Simple example

```scala
import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import akka.stream.{ActorMaterializer, Materializer}
import io.circe.Printer
import ru.tinkoff.tschema.akkaHttp.MkRoute
import io.circe.syntax._
import ru.tinkoff.tschema.swagger._

object ExampleDefinition {
  import ru.tinkoff.tschema.syntax._
  def api = get |> operation('hello) |> capture[String]('name) |> $$[String]
}

object ExampleSwagger {}

object Example extends App {
  import ExampleDefinition.api
  import akka.http.scaladsl.server.Directives._

  // building service
  object handler {
    def hello(name: String): String = s"Hello, $name"
  }

  val apiRoute = MkRoute(api)(handler)

  //building swagger
  val apiSwagger   = MkSwagger(api)(()).make(OpenApiInfo("example"))
  val printer      = Printer.spaces2.copy(dropNullValues = true)
  val swaggerRoute = path("swagger")(complete(apiSwagger.asJson.pretty(printer)))

  //typical akka http boilerplate
  implicit val system: ActorSystem        = ActorSystem("tschema-example")
  implicit val materializer: Materializer = ActorMaterializer()

  //run the server
  Http().bindAndHandle(apiRoute ~ swaggerRoute, "localhost", 8080)
}
```

More examples see in subproject `examples`

## How it works

### Definition
Your schemes definitions consist of elements from `ru.tinkoff.tschema.syntax._` and maybe custom directives

```scala
def api = get |> operation('hello) |> capture[String]('name) |> $$[String]
```

This may be read as a sequence:
1. check HTTP method is GET
2. check path prefix is "hello" and mark the following definition as part of `hello` operation
3. capture segment of uri path as `name` parameter
4. return String

Your definition could have some branching, a common prefix will be applied as the prefix of all branches.
Branching is done with the `<>` operator:
```scala
  def api =
    get |> prefix('greeting) |> capture[String]('name) |> ((
      operation('hello) |> $$[String]
    ) <> (
      operation('aloha) |> queryParam[Int]('age) |> $$[String]
    ))
```


Note that now you must implement `aloha` method in your handler
or compile error will be raised in the `MkRoute` application
### DSL
All definition elements are functions with almost no implementation, returning types from the
`ru.tinkoff.tschema.typeDSL._` package, or created by yourself.

**Those types are the definition.**

All interpreters just traversing the tree type and building construction using information from types only

Type tree will consist of type constructors, subtypes of `DSlAtom` at it branches, and `DSLRes` at leafs.
Each `DSLAtom` describes single step like parameter matching, filter or anything you'd like to do inside

### Akka http route.
When you are ready to build your source, you now can execute route building.
`MkRoute(api)(handler)` will take the type of `api` parameter and create corresponding tree.
using directive definitions given by `implicit` instances of:
 * `trait Serve.Aux[T, In]{type Out}`  where:
    *  `T` - your DSLAtom or DSLRes
    * `In` - input parameters collected by preceding `Serve` instances and tagged by names
    * `Out` - parameters, that will be provided for subtree
 * `trait RoutableIn[In, Res, Out]`  where:
    *  `In` - your DSLDef
    *  `Res` - result type, returned by corresponding method in handler
    *  `Out` - result type, defined in the API definition

You generally will need following instances:
* `ToResponseMarshaller` for returning type of your method
* `FromEntityUnmarshaller` for type, used in your `body` directive
* `FromParam` for any type in parameter directives like `queryParam`

### Swagger
When you need to create OpenApi 3.0 object, you can just apply `MkSwagger(api)(())`

This will create `SwaggerBuilder` object.
These builders could be concatenated with `++` to collect definitions from several modules
To successfully run you'll need following implicits:
* `SwaggerTypeable` for any mentioned type
* `SwaggerMapper` for any custom directive

When you final `SwaggerBuilder` is ready, apply it's `make` method supplying some top-level info,
this will create `OpenApi` object that has the [circe Encoder] instance, which you can serve any way you want.


[akka-http Routes]: https://doc.akka.io/docs/akka-http/current/routing-dsl/overview.html
[open-api-3.0]: https://swagger.io/specification/
[haskell-servant]: http://haskell-servant.readthedocs.io/en/stable/
[circe Encoder]: https://circe.github.io/circe/codec.html
[maven search]: https://search.maven.org/#search%7Cga%7C1%7Ca%3A%22typed-schema_2.12%22
[maven badge]: https://img.shields.io/maven-central/v/ru.tinkoff/typed-schema_2.12.svg
