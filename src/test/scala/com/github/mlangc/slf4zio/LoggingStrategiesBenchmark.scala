package com.github.mlangc.slf4zio

import org.scalameter.Bench
import org.scalameter.Gen
import org.slf4j.LoggerFactory
import org.scalameter.picklers.noPickler._
import org.slf4j.Logger
import org.slf4j.event.Level
import zio.DefaultRuntime
import zio.UIO
import zio.ZIO

object LoggingStrategiesBenchmark extends Bench.LocalTime with DefaultRuntime {
  private implicit class LoggerOpsAnyVal(val logger: Logger) extends AnyVal {
    def debugAnyValIO(msg: => String): UIO[Unit] = UIO {
      if (logger.isDebugEnabled)
        logger.debug(msg)
    }
  }

  private implicit class LoggerOpsLazy(logger: => Logger) {
    def debugLazyIO(msg: => String): UIO[Unit] = UIO {
      if (logger.isDebugEnabled)
        logger.debug(msg)
    }
  }

  private val nLogs = Gen.enumeration("nlogs")(1, 100)

  private lazy val debugLogger = getLogger(Level.DEBUG)
  private lazy val infoLogger = getLogger(Level.INFO)

  private def logStr = "--benchmark me--"

  performance of "debugLogger" in {
    measure method "debug" in {
      using(nLogs) in nTimes(debugLogger.debug(logStr))
    }

    measure method "debugAnyValIO" in {
      using(nLogs) in runNtimesIO(debugLogger.debugAnyValIO(logStr))
    }

    measure method "debugLazyIO" in {
      using(nLogs) in runNtimesIO(debugLogger.debugLazyIO(logStr))
    }
  }

  performance of "infoLogger" in {
    measure method "debug" in {
      using(nLogs) in nTimes(infoLogger.debug(logStr))
    }

    measure method "debugAnyValIO" in {
      using(nLogs) in runNtimesIO(infoLogger.debugAnyValIO(logStr))
    }

    measure method "debugLazyIO" in {
      using(nLogs) in runNtimesIO(infoLogger.debugLazyIO(logStr))
    }
  }

  private def nTimes(block: => Unit)(n: Int): Unit = {
    1.to(n).foreach(_ => block)
  }

  private def getLogger(level: Level): Logger =
    LoggerFactory.getLogger(getClass.getCanonicalName + "." + level.toString.toLowerCase)

  private def runNtimesIO(io: UIO[Unit])(n: Int): Unit = {
    unsafeRun(nTimesIO(n)(io))
  }

  private def nTimesIO[R, E](n: Int)(io: ZIO[R, E, Unit]): ZIO[R, E, Unit] = n match {
    case 1 => io
    case n if n > 1 => io *> nTimesIO(n - 1)(io)
    case _ => ZIO.unit
  }
}
