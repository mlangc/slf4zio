# SLF4ZIO
*Integrates SLF4J with ZIO in the simplest possible manner.*

[![Maven Central](https://maven-badges.herokuapp.com/maven-central/com.github.mlangc/slf4zio_2.12/badge.svg)](https://maven-badges.herokuapp.com/maven-central/com.github.mlangc/slf4zio_2.12)

## When to Use
If your code is based on [ZIO](https://zio.dev/) and you want to log with [SLF4J](https://www.slf4j.org/).

## When not to Use
If you want to track logging effects using the [ZIO Environment](http://degoes.net/articles/zio-environment)
try [zio-slf4j](https://github.com/NeQuissimus/zio-slf4j). If you are into Tagless Final,
take a look at [log4cats](https://github.com/ChristopherDavenport/log4cats).

## How to Use
### Creating loggers safely as needed
```scala
import com.github.mlangc.slf4zio.api._
import zio.Task
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
```

### Using the LoggingSupport convenience trait
````scala
import zio.TaskR
import zio.random
import zio.random.Random
import com.github.mlangc.slf4zio.api._

class SomeClass extends LoggingSupport {
  def doStuff: TaskR[Random, Unit] =
    for {
      _ <- logger.warnIO("What the heck")
      _ <- random.nextBoolean.flatMap {
        case true => logger.infoIO("Uff, that was close")
        case false => logger.errorIO("Game over", new IllegalStateException("This is the end"))
      }
    } yield ()
}
````
Note that the `logger` field in the `LoggingSupport` trait is lazy. Since the implicit class
that implements the various `**IO` methods, like `debugIO`, `infoIO` and so forth, wraps the
logger using a [by-name parameter](https://docs.scala-lang.org/tour/by-name-parameters.html),
logger initialization won't happen before `unsafeRun`, if you don't access the logger directly
from non effectful code. To ensure referential transparency for creating an object of a class that
inherits the `LoggingSupport` trait, you have wrap it's creation in an effect of its own.
For practical purposes, you might consider obtaining a logger to be a pure operation as soon as the
logging framework has finished its initialization, and not care too much about this subtlety.





