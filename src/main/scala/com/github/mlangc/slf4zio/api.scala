package com.github.mlangc.slf4zio

import org.slf4j.{Logger, LoggerFactory}
import zio.{UIO, ZIO}

import scala.concurrent.duration.Duration
import scala.reflect.ClassTag

object api {
  implicit final class Slf4jLoggerOps(logger: => Logger) {
    def debugIO(msg: => String): UIO[Unit] = UIO {
      if (logger.isDebugEnabled)
        logger.debug(msg)
    }

    def infoIO(msg: => String): UIO[Unit] = UIO {
      if (logger.isInfoEnabled())
        logger.info(msg)
    }

    def warnIO(msg: => String): UIO[Unit] = UIO {
      if (logger.isWarnEnabled())
        logger.warn(msg)
    }

    def errorIO(msg: => String): UIO[Unit] = UIO {
      if (logger.isErrorEnabled())
        logger.error(msg)
    }

    def debugIO(msg: => String, th: Throwable): UIO[Unit] = UIO {
      if (logger.isDebugEnabled)
        logger.debug(msg, th)
    }

    def infoIO(msg: => String, th: Throwable): UIO[Unit] = UIO {
      if (logger.isInfoEnabled)
        logger.debug(msg, th)
    }

    def warnIO(msg: => String, th: Throwable): UIO[Unit] = UIO {
      if (logger.isWarnEnabled)
        logger.warn(msg, th)
    }

    def errorIO(msg: => String, th: Throwable): UIO[Unit] = UIO {
      if (logger.isErrorEnabled)
        logger.error(msg, th)
    }
  }

  def getLogger[T](implicit classTag: ClassTag[T]): Logger =
    getLogger(classTag.runtimeClass)

  def getLogger(clazz: Class[_]): Logger =
    LoggerFactory.getLogger(clazz)

  def getLogger(name: String): Logger =
    LoggerFactory.getLogger(name)

  def makeLogger(name: String): UIO[Logger] =
    UIO(getLogger(name))

  def makeLogger[T](implicit classTag: ClassTag[T]): UIO[Logger] =
    UIO(getLogger[T])

  trait LoggingSupport {
    @transient
    protected final lazy val logger: Logger = getLogger(getClass)

    protected implicit final class ZioLoggerOps[R, E, A](zio: ZIO[R, E, A]) {
      def logDebugPerformance(msg: Duration => String, threshold: Duration = Duration.Zero): ZIO[R, E, A] = {
        def instrumented: ZIO[R, Nothing, Either[E, A]] = for {
          nanosBefore <- UIO(System.nanoTime())
          aOrE <- zio.either
          nanosAfter <- UIO(System.nanoTime())
          elapsed = Duration.fromNanos(nanosAfter - nanosBefore)
          _ <- ZIO.when(elapsed >= threshold)(logger.debugIO(msg(elapsed)))
        } yield aOrE

        for {
          debugEnabled <- UIO(logger.isDebugEnabled)
          a <- if (debugEnabled) instrumented.absolve else zio
        } yield a
      }
    }

    protected final def logDebugPerformance[A](msg: Duration => String, threshold: Duration = Duration.Zero)(thunk: => A): A = {
      if (!logger.isDebugEnabled) thunk else {
        val nanosBefore = System.nanoTime()
        try thunk finally {
          val nanosAfter = System.nanoTime()
          val elapsed = Duration.fromNanos(nanosAfter - nanosBefore)

          if (elapsed >= threshold)
            logger.debug(msg(elapsed))
        }
      }
    }
  }
}
