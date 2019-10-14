package com.github.mlangc.slf4zio

import zio.test.Assertion._
import zio.test.DefaultRunnableSpec
import zio.test._

object ReadmeExamplesTest extends DefaultRunnableSpec(
  suite("ReadmeExamplesTest")(
    testM("creating loggins as needed") {
      val effect = {
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

      assertM(effect, isUnit)
    },
    testM("Using the convenience trait") {
      val effect = {
        import com.github.mlangc.slf4zio.api._
        import zio.RIO
        import zio.random
        import zio.random.Random

        class SomeClass extends LoggingSupport {
          def doStuff: RIO[Random, Unit] =
            for {
              _ <- logger.warnIO("What the heck")
              _ <- random.nextBoolean.flatMap {
                case true => logger.infoIO("Uff, that was close")
                case false => logger.errorIO("Game over", new IllegalStateException("This is the end"))
              }
            } yield ()
        }

        new SomeClass().doStuff
      }

      assertM(effect, isUnit)
    }
  )
)
