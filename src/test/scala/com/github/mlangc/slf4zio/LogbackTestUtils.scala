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
import zio.scheduler.Scheduler

object LogbackTestUtils {
  def waitForLogbackInitialization: IO[LogbackInitializationTimeout, Unit] = {
    val schedule: Schedule[Clock, Boolean, (Boolean, Duration)] =
      (Schedule.doUntil[Boolean](identity) <* (Schedule.spaced(500.micros) <* Schedule.recurs(20))) && Schedule.elapsed

    logbackInitialized.repeat(schedule).flatMap {
      case (true, _) => ZIO.unit
      case (false, elapsed) => ZIO.fail(LogbackInitializationTimeout(elapsed))
    }.provideLayer(Scheduler.live >>> Clock.live)
  }

  def logbackInitialized: UIO[Boolean] = UIO {
    LoggerFactory.getILoggerFactory.isInstanceOf[LoggerContext]
  }
}
