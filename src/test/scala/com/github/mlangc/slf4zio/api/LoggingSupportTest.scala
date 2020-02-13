package com.github.mlangc.slf4zio.api

import ch.qos.logback.classic.spi.ILoggingEvent
import com.github.mlangc.slf4zio.LogbackTestAppender
import zio.UIO
import zio.ZIO
import zio.test.DefaultRunnableSpec
import zio.test._

object LoggingSupportTest extends DefaultRunnableSpec with LoggingSupport {
  def spec = suite("LoggingSupport")(
    testM("Make sure something is logged") {
      for {
        _ <- logger.infoIO("Test")
        evts <- getLogEvents
      } yield assert(evts)(Assertion.isNonEmpty)
    },
    suite("Performance logging")(
      suite("ZIO syntax")(
        testM("Success only") {
          for {
            _ <- ZIO.unit.perfLog(LogSpec.onSucceed(d => s"Simple $d"))
            _ <- ZIO.succeed(42).perfLog(LogSpec.onSucceed((d, a) => s"Simple ($d, $a)"))
            evts <- getLogEvents(_.getMessage.startsWith("Simple"))
          } yield assert(evts.size)(Assertion.equalTo(2))
        }
      )
    )
  )

  private def getLogEvents(p: ILoggingEvent => Boolean): UIO[List[ILoggingEvent]] =
    LogbackTestAppender.events.map { evts =>
      evts.filter(evt => p(evt) && evt.getLoggerName.contains(getClass.getSimpleName))
    }

  private def getLogEvents: UIO[List[ILoggingEvent]] =
    getLogEvents(_ => true)
}
