package com.github.mlangc.slf4zio

import org.slf4j.Logger
import org.slf4j.LoggerFactory
import org.slf4j.event.Level
import zio.Cause
import zio.Has
import zio.UIO
import zio.URIO
import zio.ZIO
import zio.clock
import zio.clock.Clock
import zio.duration.Duration

import scala.reflect.ClassTag

package object api {
  implicit final class Slf4jLoggerOps(logger: => Logger) {
    def traceIO(msg: => String): UIO[Unit] = UIO {
      if (logger.isTraceEnabled())
        logger.trace(msg)
    }

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

    def traceIO(msg: => String, th: Throwable): UIO[Unit] = UIO {
      if (logger.isTraceEnabled())
        logger.trace(msg, th)
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

    def logIO(msg: LogMessage): UIO[Unit] =
      UIO(log(msg))

    def log(msg: LogMessage): Unit =
      if (msg.suppressed) () else msg.level match {
        case Level.ERROR => logger.error(msg.text)
        case Level.WARN => logger.warn(msg.text)
        case Level.INFO => logger.info(msg.text)
        case Level.DEBUG => logger.debug(msg.text)
        case Level.TRACE => logger.trace(msg.text)
      }
  }

  type Logging = Has[Logging.Service[Any]]

  object logging extends Logging.Service[Logging] {
    def logger: URIO[Logging, Logger] =
      ZIO.accessM[Logging](_.get.logger)
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

  implicit final class LogMessageInterpolator(val stringContext: StringContext) extends AnyVal {
    def trace(args: Any*): LogMessage = {
      LogMessage.trace(stringContext.s(args: _*))
    }

    def debug(args: Any*): LogMessage = {
      LogMessage.debug(stringContext.s(args: _*))
    }

    def info(args: Any*): LogMessage = {
      LogMessage.info(stringContext.s(args: _*))
    }

    def warn(args: Any*): LogMessage = {
      LogMessage.warn(stringContext.s(args: _*))
    }

    def error(args: Any*): LogMessage = {
      LogMessage.error(stringContext.s(args: _*))
    }
  }

  implicit final class ZioLoggingOps[R, E, A](val zio: ZIO[R, E, A]) extends AnyVal {
    def perfLogR(spec: LogSpec[E, A]): ZIO[R with Logging with Clock, E, A] =
      if (spec.isNoOp) zio else clock.nanoTime.flatMap { t0 =>
        def handleError(cause: Cause[E]): ZIO[Logging with Clock, E, Nothing] =
          if (spec.onError.isEmpty && spec.onTermination.isEmpty) ZIO.halt(cause)
          else clock.nanoTime.flatMap { t1 =>
            val d = Duration.fromNanos(t1 - t0)

            val msgs = cause.failureOrCause match {
              case Right(failure) => spec.onTermination.map(_ (d, failure))
              case Left(e) => spec.onError.map(_ (d, e))
            }

            ZIO.foreach(msgs)(m => logging.logIO(m)) *> ZIO.halt(cause)
          }

        def handleSuccess(a: A): ZIO[R with Logging with Clock, Nothing, A] =
          if (spec.onSucceed.isEmpty) ZIO.succeed(a) else {
            for {
              t1 <- clock.nanoTime
              d = Duration.fromNanos(t1 - t0)
              msgs = spec.onSucceed.map(_ (d, a))
              _ <- ZIO.foreach(msgs)(m => logging.logIO(m))
            } yield a
          }

        zio.foldCauseM(handleError, handleSuccess)
      }
  }
}
