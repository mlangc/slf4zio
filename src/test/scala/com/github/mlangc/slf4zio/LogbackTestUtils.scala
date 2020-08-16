package com.github.mlangc.slf4zio

import ch.qos.logback.classic.LoggerContext
import org.slf4j.LoggerFactory
import zio.IO
import zio.Schedule
import zio.UIO
import zio.ZIO
import zio.clock.Clock
import zio.duration.Duration
import zio.duration.durationInt

object LogbackTestUtils {
  def waitForLogbackInitialization: IO[LogbackInitializationTimeout, Unit] = {
    val schedule: Schedule[Clock, Boolean, (Boolean, Duration)] =
      (Schedule.recurUntil[Boolean](identity) <* (Schedule.spaced(1.milli) <* Schedule.recurs(500))) && Schedule.elapsed

    logbackInitialized.repeat(schedule).flatMap {
      case (true, _) => ZIO.unit
      case (false, elapsed) => ZIO.fail(LogbackInitializationTimeout(elapsed))
    }.provideLayer(Clock.live)
  }

  def logbackInitialized: UIO[Boolean] = UIO {
    LoggerFactory.getILoggerFactory.isInstanceOf[LoggerContext]
  }
}
