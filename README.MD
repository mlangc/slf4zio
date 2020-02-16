# SLF4ZIO
*Integrates SLF4J with ZIO in the simplest possible manner.*

[![Latest Release](https://img.shields.io/maven-central/v/com.github.mlangc/slf4zio_2.13?color=green&label=latest-release)](https://oss.sonatype.org/content/repositories/releases/com/github/mlangc/slf4zio_2.13)
[![Latest Snapshot](https://img.shields.io/nexus/s/com.github.mlangc/slf4zio_2.13?label=latest-snapshot&server=https%3A%2F%2Foss.sonatype.org)](https://oss.sonatype.org/content/repositories/snapshots/com/github/mlangc/slf4zio_2.13/)

## When to Use
If your code is based on [ZIO](https://zio.dev/) and you want to log with [SLF4J](https://www.slf4j.org/).

## When not to Use
If you want to track logging effects using the [ZIO Environment](http://degoes.net/articles/zio-environment) exclusively, consider using
[zio-slf4j](https://github.com/NeQuissimus/zio-slf4j). If you are into Tagless Final,
take a look at [log4cats](https://github.com/ChristopherDavenport/log4cats).

## How to Use
### Creating loggers safely as needed
```scala
import com.github.mlangc.slf4zio.api._
import zio.Task

val effect: Task[Unit] = {
    // ...
    class SomeClass
    // ...
    for {
      logger <- makeLogger[SomeClass]
      _ <- logger.debugIO("Debug me tender")
      // ...
      _ <- Task {
        // Note that makeLogger just returns a plain SLF4J logger; you can therefore use it from
        // effectful code directly:
        logger.info("Don't be shy")
        // ...
        logger.warn("Please take me home")
      }
    } yield ()
}
```

### Using the Logging service
```scala
import com.github.mlangc.slf4zio.api._
import zio.ZIO
import zio.Task

val effect: ZIO[Logging, Throwable, Unit] = {
    for {
      _ <- logging.warnIO("Surprise, surprise")
      plainLogger <- logging.logger
      _ <- Task {
        plainLogger.debug("Shhh...")
        plainLogger.warn("The devil always comes in disguise")
      }
      _ <- logging.traceIO("...")
    } yield ()
}
```

### Using the LoggingSupport convenience trait
````scala
import com.github.mlangc.slf4zio.api._
import zio.RIO
import zio.random
import zio.random.Random

val effect: RIO[Random, Unit] = {
    object SomeObject extends LoggingSupport {
      def doStuff: RIO[Random, Unit] =
        for {
          _ <- logger.warnIO("What the heck")
          _ <- random.nextBoolean.flatMap {
            case true => logger.infoIO("Uff, that was close")
            case false => logger.errorIO("Game over", new IllegalStateException("This is the end"))
          }
        } yield ()
    }

    SomeObject.doStuff
}
````

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





