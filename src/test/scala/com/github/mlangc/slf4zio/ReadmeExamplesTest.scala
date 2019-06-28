package com.github.mlangc.slf4zio

import org.scalatest.FreeSpec
import zio.DefaultRuntime

class ReadmeExamplesTest extends FreeSpec with DefaultRuntime {
  "Creating loggers as needed" in {
    unsafeRun {
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
    }
  }

  "Using the convenience trait" in {
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

    unsafeRun {
      new SomeClass().doStuff
    }
  }
}
