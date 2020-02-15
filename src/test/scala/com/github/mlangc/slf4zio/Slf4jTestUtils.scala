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
import zio.test.TestFailure

object Slf4jTestUtils {
  def provideSlf4jInitialized[R]: ZIO[R, TestFailure[Throwable], R] = {
    ZIO.fromFunctionM(r => waitForSlf4jInitialization.as(r))
  }

  def waitForSlf4jInitialization: IO[TestFailure[Throwable], Unit] = {
    val schedule: Schedule[Clock, Boolean, (Boolean, Duration)] =
      (Schedule.doUntil[Boolean](identity) <* (Schedule.spaced(500.micros) <* Schedule.recurs(32))) && Schedule.elapsed

    slf4jInitialized.repeat(schedule).flatMap {
      case (true, _) => ZIO.unit
      case (false, elapsed) => ZIO.fail(TestFailure.fail(new RuntimeException(s"SLF4J still not initialized after ${elapsed.render}")))
    }.provideLayer(Scheduler.live >>> Clock.live)
  }

  def slf4jInitialized: UIO[Boolean] = UIO {
    LoggerFactory.getILoggerFactory.isInstanceOf[LoggerContext]
  }
}
