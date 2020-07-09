package com.github.mlangc.slf4zio

import com.github.mlangc.slf4zio.api.Logging
import zio.test.Assertion._
import zio.test.DefaultRunnableSpec
import zio.test._
import zio.test.environment.live

object ReadmeExamplesTest extends DefaultRunnableSpec {
  def spec = suite("ReadmeExamplesTest")(
    testM("creating loggers as needed") {
      import com.github.mlangc.slf4zio.api._
      import zio.duration.durationInt
      import zio.clock.Clock
      import zio.RIO
      import zio.ZIO
      import zio.Task

      val effect: RIO[Clock, Unit] = {
        // ...
        class SomeClass
        // ...
        for {
          logger <- makeLogger[SomeClass]
          marker <- makeMarker("[MARKER]")
          _ <- logger.debugIO("Debug me tender")
          // ...
          _ <- Task {
            // Note that makeLogger just returns a plain SLF4J logger; you can therefore use it from
            // effectful code directly:
            logger.info("Don't be shy")
            // ...
            logger.warn("Please take me home")
            // ...
            logger.info(marker, "Don't worry")
            // ...
          }
          // ...
          _ <- logger.perfLogZIO(ZIO.sleep(10.millis))(
            // See below for more examples with `LogSpec`
            LogSpec.onSucceed(d => info"Feeling relaxed after sleeping ${d.render}"))
        } yield ()
      }

      assertM(live(effect))(isUnit)
    },
    testM("Using the convenience trait") {
      import zio._
      import zio.random
      import zio.random.Random
      import zio.clock.Clock
      import zio.duration.durationInt
      import com.github.mlangc.slf4zio.api._

      object SomeObject extends LoggingSupport {
        def doStuff: RIO[Random with Clock, Unit] = {
          for {
            _ <- logger.warnIO("What the heck")
            marker <- makeMarker("[MARKER]")
            _ <- logger.debugIO(marker, "Wat?")
            _ <- ZIO.ifM(random.nextBoolean)(
              logger.infoIO("Uff, that was close"),
              logger.errorIO("Game over", new IllegalStateException("This is the end"))
            )

            _ <- Task {
              // logger is just a plain SLF4J logger; you can therefore use it from
              // effectful code directly:
              logger.trace("Wink wink nudge nudge")
            }

            _ <- ZIO.sleep(8.millis).as(23).perfLog(
              // See below for more examples with `LogSpec`
              LogSpec.onSucceed[Int]((d, i) => debug"Finally done with $i after ${d.render}")
                .withThreshold(5.millis)
            )
          } yield ()
        }
      }

      assertM(live(SomeObject.doStuff.ignore))(isUnit)
    },
    testM("Using the service") {
      import com.github.mlangc.slf4zio.api._
      import zio.RIO
      import zio.ZIO
      import zio.Task
      import zio.clock.Clock

      val effect: RIO[Logging with Clock, Unit] =
        for {
          _ <- logging.warnIO("Surprise, surprise")
          plainLogger <- logging.logger
          _ <- Task {
            plainLogger.debug("Shhh...")
            plainLogger.warn("The devil always comes in disguise")
          }
          _ <- logging.traceIO("...")
          marker <- makeMarker("[MARKER]")
          _ <- logging.debugIO(marker, "Here we are")
          getNumber = ZIO.succeed(42)
          // See below for more examples with `LogSpec`
          _ <- getNumber.perfLogZ(LogSpec.onSucceed(d => debug"Got number after ${d.render}"))
        } yield ()

      assertM(effect)(isUnit)
    },
    testM("Performance Logging - Using the Logging Service") {
      import com.github.mlangc.slf4zio.api._
      import zio.ZIO
      import zio.clock.Clock
      import zio.duration.durationInt

      // Simple specs can be combined using the `++` to obtain more complex specs
      val logSpec1: LogSpec[Throwable, Int] =
        LogSpec.onSucceed[Int]((d, a) => info"Succeeded after ${d.render} with $a") ++
          LogSpec.onError[Throwable]((d, th) => error"Failed after ${d.render} with $th") ++
          LogSpec.onTermination((d, c) => error"Fatal failure after ${d.render}: ${c.prettyPrint}")

      // A threshold can be applied to a LogSpec. Nothing will be logged, unless the threshold is exceeded.
      val logSpec2: LogSpec[Any, Any] =
        LogSpec.onSucceed(d => warn"Operation took ${d.render}")
          .withThreshold(1.milli)

      // Will behave like logSpec1 and eventually log a warning as specified in logSpec2
      val logSpec3: LogSpec[Throwable, Int] = logSpec1 ++ logSpec2

      val effect: ZIO[Clock with Logging, Nothing, Unit] = for {
        _ <- ZIO.sleep(5.micros).perfLogZ(LogSpec.onSucceed(d => debug"Done after ${d.render}"))
        _ <- ZIO.sleep(1.milli).as(42).perfLogZ(logSpec1)
        _ <- ZIO.sleep(2.milli).perfLogZ(logSpec2)
        _ <- ZIO.sleep(3.milli).as(23).perfLogZ(logSpec3)
      } yield ()

      assertM(effect.provideSomeLayer[Logging](Clock.live))(isUnit)
    }
  ).provideLayer(Logging.forClass(getClass) ++ environment.TestEnvironment.any)
}
