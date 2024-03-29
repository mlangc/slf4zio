# SLF4ZIO
*Integrates SLF4J with ZIO in a simple manner.*

[![Latest Release](https://img.shields.io/maven-central/v/com.github.mlangc/slf4zio_2.13?color=green&label=latest-release)](https://oss.sonatype.org/content/repositories/releases/com/github/mlangc/slf4zio_2.13)
[![Latest Snapshot](https://img.shields.io/nexus/s/com.github.mlangc/slf4zio_2.13?label=latest-snapshot&server=https%3A%2F%2Foss.sonatype.org)](https://oss.sonatype.org/content/repositories/snapshots/com/github/mlangc/slf4zio_2.13/)

## When to Use
If your code is based on [ZIO](https://zio.dev/) and you want to log with [SLF4J](https://www.slf4j.org/) without additional abstractions getting
in your way.

## How to Use
The library supports three different coding styles, that you can mix and match according to your needs. They are listed here in the order of
my personal preference:

### 1. Using the LoggingSupport Convenience Trait

````scala
import com.github.mlangc.slf4zio.api._
import zio._

object SomeObject extends LoggingSupport {
  def doStuff: Task[Unit] = {
    for {
      _ <- logger.warnIO("What the heck")
      _ <- ZIO.ifZIO(Random.nextBoolean)(
        logger.infoIO("Uff, that was close"),
        logger.errorIO("Game over", new IllegalStateException("This is the end"))
      )

      _ <- ZIO.attempt {
        // logger is just a plain SLF4J logger; you can therefore use it from
        // effectful code directly:
        logger.trace("Wink wink nudge nudge")
      }

      _ <- ZIO
        .sleep(8.millis)
        .as(23)
        .perfLog(
          // See below for more examples with `LogSpec`
          LogSpec
            .onSucceed[Int]((d, i) => debug"Finally done with $i after ${d.render}")
            .withThreshold(5.millis)
        )
    } yield ()
  }
}
````

#### Side Note
Note that the `logger` field in the `LoggingSupport` trait is lazy. Since the implicit class
that implements the various `**IO` methods, like `debugIO`, `infoIO` and so forth, wraps the
logger using a [by-name parameter](https://docs.scala-lang.org/tour/by-name-parameters.html),
logger initialization won't happen before `unsafeRun`, if you don't access the logger directly
from non effectful code. To ensure referential transparency for creating an object of a class that
inherits the `LoggingSupport` trait even with outright broken or strange logger implementations,
you have wrap the creation of the object in an effect of its own. It might make more sense to use
another logger implementation though. For practical purposes, I would consider obtaining a 
logger to be a pure operation as soon as the logging framework has finished its initialization,
and not care too much about this subtlety.

### 2. Creating Loggers as Needed

```scala
import com.github.mlangc.slf4zio.api._
import zio._

val effect: Task[Unit] = {
  // ...
  class SomeClass
  // ...
  for {
    logger <- makeLogger[SomeClass]
    _ <- logger.debugIO("Debug me tender")
    // ...
    _ <- ZIO.attempt {
      // Note that makeLogger just returns a plain SLF4J logger; you can therefore use it from
      // effectful code directly:
      logger.info("Don't be shy")
      // ...
      logger.warn("Please take me home")
      // ...
    }
    // ...
    _ <- logger.perfLogZIO(ZIO.sleep(10.millis))(
      // See below for more examples with `LogSpec`
      LogSpec.onSucceed(d => info"Feeling relaxed after sleeping ${d.render}")
    )
  } yield ()
}
```

### 3. Using the Logging Service

```scala
import com.github.mlangc.slf4zio.api._
import zio._

val effect: RIO[Logging, Unit] =
  for {
    _ <- logging.warnIO("Surprise, surprise")
    plainLogger <- logging.logger
    _ <- ZIO.attempt {
      plainLogger.debug("Shhh...")
      plainLogger.warn("The devil always comes in disguise")
    }
    _ <- logging.traceIO("...")
    getNumber = ZIO.succeed(42)
    // See below for more examples with `LogSpec`
    _ <- getNumber.perfLogZ(LogSpec.onSucceed(d => debug"Got number after ${d.render}"))
  } yield ()
```      

### Using Markers

```scala
import com.github.mlangc.slf4zio.api._
import zio._

val effect: RIO[Logging, Unit] =
  for {
    marker <- getMarker("[MARKER]")
    _ <- logging.infoIO(marker, "Here we are")
    logger <- logging.logger
    _ <- logger.debugIO(marker, "Wat?")
    _ <- ZIO.attempt {
      logger.warn(marker, "Don't worry")
    }
  } yield ()
```    

### Performance Logging
Apart from providing ZIO aware wrappers for [SLF4J](https://www.slf4j.org/), the library might also
help you with performance related logging. The examples from above are meant to give you the overall
idea. Here is another snippet, that is meant to illustrate how to build complex `LogSpec`s from simple
ones, utilizing the underlying monoidial structure:

```scala
import com.github.mlangc.slf4zio.api._
import zio._

// Simple specs can be combined using the `++` to obtain more complex specs
val logSpec1: LogSpec[Throwable, Int] =
  LogSpec.onSucceed[Int]((d, a) => info"Succeeded after ${d.render} with $a") ++
    LogSpec.onError[Throwable]((d, th) => error"Failed after ${d.render} with $th") ++
    LogSpec.onTermination((d, c) => error"Fatal failure after ${d.render}: ${c.prettyPrint}")

// A threshold can be applied to a LogSpec. Nothing will be logged, unless the threshold is exceeded.
val logSpec2: LogSpec[Any, Any] =
  LogSpec
    .onSucceed(d => warn"Operation took ${d.render}")
    .withThreshold(1.milli)

// Will behave like logSpec1 and eventually log a warning as specified in logSpec2
val logSpec3: LogSpec[Throwable, Int] = logSpec1 ++ logSpec2

val effect: ZIO[Logging, Nothing, Unit] = for {
  _ <- ZIO.sleep(5.micros).perfLogZ(LogSpec.onSucceed(d => debug"Done after ${d.render}"))
  _ <- ZIO.sleep(1.milli).as(42).perfLogZ(logSpec1)
  _ <- ZIO.sleep(2.milli).perfLogZ(logSpec2)
  _ <- ZIO.sleep(3.milli).as(23).perfLogZ(logSpec3)
} yield ()
```     

### Using MDC convenience APIs
SLF4ZIO also ships with a set of convenience APIs for `org.slf4j.MDC`. Note however, that traditional
MDC implementations are based on thread local data, which doesn't work at all with ZIO, where a
single `zio.Fiber` might run on different threads during its lifetime, and a single thread might 
accommodate multiple fibers. **If you want to use MDC logging in your ZIO based application, it is
critical to use a fiber aware MDC implementation, as provided for example by 
[zio-interop-log4j2](https://github.com/mlangc/zio-interop-log4j2). `MDZIO` is just a collection of
convenience APIs for interacting with `org.slf4j.MDC` that doesn't add any functionality of its own.**

## Alternatives
If you want to track logging effects using the [ZIO Environment](http://degoes.net/articles/zio-environment) exclusively, consider using
[zio-logging](https://github.com/zio/zio-logging). If you are into Tagless Final,
take a look at [log4cats](https://github.com/ChristopherDavenport/log4cats).







