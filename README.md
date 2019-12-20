| CI | Release | 
| --- | --- |
| [![Build Status](https://travis-ci.com/TinkoffCreditSystems/typed-schema.svg?branch=master)](https://travis-ci.com/TinkoffCreditSystems/typed-schema) | [![Maven Central](https://img.shields.io/maven-central/v/ru.tinkoff/typed-schema-typedsl_2.12.svg)](https://search.maven.org/search?q=ru.tinkoff.typed-schema-typedsl) | 

# Welcome to Typed Schema
Typed schema is an http service definition DSL, currently translating to [akka-http Routes](https://doc.akka.io/docs/akka-http/current/routing-dsl/overview.html) and [OpenApi 3.0 definition](https://swagger.io/specification/) inspired by the [haskell-servant](http://haskell-servant.readthedocs.io/en/stable/) library.

## Getting started
Typed Schema is published to Maven Central and cross-built for Scala `2.12` and `2.13` so you can just add the following to your build:

```scala
libraryDependencies ++= Seq(
  "ru.tinkoff" %% "typed-schema" % "latest version in badge"
)
```

## Motivation
We the People building services using modern scala often struggling to satisfy following requirements
* Service implementation should be checked to be compatible with OpenApi 3.0 specifications at the compile time
* Service definition should be detachable from the implementation and exportable as mere specification
* There should be an easy way to migrate all the services to different effect\future\task implementation
without changing any definition
* There should be some way to migrate all the service to another framework without reimplementing them

# [Learn More on the TypedSchema Microsite](https://tinkoffcreditsystems.github.io/typed-schema/)
