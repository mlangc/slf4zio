package com.github.mlangc.slf4zio

import zio.test.Assertion._
import zio.test.DefaultRunnableSpec
import zio.test._

object ReadmeExamplesTest extends DefaultRunnableSpec {
  def spec = suite("ReadmeExamplesTest")(
    testM("creating loggers as needed") {
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

      assertM(effect)(isUnit)
    },
    testM("Using the convenience trait") {
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

      assertM(effect)(isUnit)
    },
    testM("Using the service") {
      import com.github.mlangc.slf4zio.api._
      import zio.Task
      import zio.ZIO

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

      assertM(effect)(isUnit)
    }.provideManaged {
      import com.github.mlangc.slf4zio.api._
      import zio.UIO

      UIO {
        class SomeClass

        new Logging.ForClass {
          protected def loggingClass = classOf[SomeClass]
        }
      }.toManaged_
    })
}
