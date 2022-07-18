package com.github.mlangc.slf4zio

import ch.qos.logback.classic.LoggerContext
import org.slf4j.LoggerFactory
import zio.{Duration, IO, Schedule, UIO, ZIO, durationInt}

object LogbackTestUtils {
  def waitForLogbackInitialization: IO[LogbackInitializationTimeout, Unit] = {
    val schedule: Schedule[Any, Boolean, (Boolean, Duration)] =
      (Schedule.recurUntil[Boolean](identity) <* (Schedule.spaced(1.milli) <* Schedule.recurs(500))) && Schedule.elapsed

    logbackInitialized.repeat(schedule).flatMap {
      case (true, _) => ZIO.unit
      case (false, elapsed) => ZIO.fail(LogbackInitializationTimeout(elapsed))
    }
  }

  def logbackInitialized: UIO[Boolean] = ZIO.succeed {
    LoggerFactory.getILoggerFactory.isInstanceOf[LoggerContext]
  }
}
