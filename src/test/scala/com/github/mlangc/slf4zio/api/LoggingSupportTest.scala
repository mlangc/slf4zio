package com.github.mlangc.slf4zio.api

import ch.qos.logback.classic.spi.ILoggingEvent
import ch.qos.logback.classic.Level
import com.github.mlangc.slf4zio.LogbackInitializationTimeout
import com.github.mlangc.slf4zio.LogbackTestAppender
import com.github.mlangc.slf4zio.LogbackTestUtils
import zio.duration2DurationOps
import zio.test._
import zio.test.Assertion
import zio.test.Assertion.equalTo
import zio.test.Assertion.hasSize
import zio.test.ZIOSpecDefault
import zio.IO
import zio.ZIO

object LoggingSupportTest extends ZIOSpecDefault with LoggingSupport {
  def spec = suite("LoggingSupport")(
    test("Make sure something is logged") {
      for {
        _ <- logger.infoIO("Test")
        evts <- getLogEvents
      } yield assert(evts)(Assertion.isNonEmpty)
    },
    suite("Performance logging")(
      suite("ZIO syntax")(
        test("Success only") {
          for {
            _ <- ZIO.unit.perfLog(LogSpec.onSucceed(d => debug"Simple ${d.render}"))
            _ <- ZIO.succeed(42).perfLog(LogSpec.onSucceed((d, a) => debug"Simple (${d.render}, $a)"))
            evts <- getLogEvents(_.getMessage.startsWith("Simple"))
          } yield assert(evts.size)(equalTo(2))
        },
        test("Success and failures") {
          val spec: LogSpec[Throwable, Any] = LogSpec.onSucceed(d => info"Success after ${d.render}") ++
            LogSpec.onError[Throwable]((d, e) => warn"Error $e after ${d.render}") ++
            LogSpec.onTermination((d, c) => error"Fatal error ${c.prettyPrint} after ${d.render}")

          for {
            _ <- ZIO.unit.perfLog(spec)
            _ <- ZIO.fail(new RuntimeException).perfLog(spec).ignore
            _ <- ZIO.die(new RuntimeException).perfLog(spec).catchAllCause(_ => ZIO.unit)
            succEvents <- getLogEvents(evt => evt.getLevel == Level.INFO && evt.getMessage.startsWith("Success"))
            errEvents <- getLogEvents(evt => evt.getLevel == Level.WARN && evt.getMessage.startsWith("Error"))
            termEvents <- getLogEvents(evt => evt.getLevel == Level.ERROR && evt.getMessage.startsWith("Fatal"))
          } yield {
            assert(succEvents)(hasSize(equalTo(1))) &&
            assert(errEvents)(hasSize(equalTo(1))) &&
            assert(termEvents)(hasSize(equalTo(1)))
          }
        }
      )
    )
  ) @@ TestAspect.before(live(LogbackTestUtils.waitForLogbackInitialization).orDie)

  private def getLogEvents(p: ILoggingEvent => Boolean): IO[LogbackInitializationTimeout, List[ILoggingEvent]] =
    LogbackTestAppender.events.map { evts =>
      evts.filter(evt => p(evt) && evt.getLoggerName.contains(getClass.getSimpleName))
    }

  private def getLogEvents: IO[LogbackInitializationTimeout, List[ILoggingEvent]] =
    getLogEvents(_ => true)
}
