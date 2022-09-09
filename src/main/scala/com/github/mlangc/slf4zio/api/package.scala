package com.github.mlangc.slf4zio

import org.slf4j.event.Level
import org.slf4j.Logger
import org.slf4j.LoggerFactory
import org.slf4j.Marker
import org.slf4j.MarkerFactory
import scala.reflect.ClassTag
import scala.util.Failure
import scala.util.Success
import scala.util.Try
import zio.Cause
import zio.Clock
import zio.Duration
import zio.UIO
import zio.ULayer
import zio.URIO
import zio.ZIO
import zio.ZLayer

package object api {
  implicit final class Slf4jLoggerOps(logger: => Logger) {
    def traceIO(msg: => String): UIO[Unit] = ZIO.succeed {
      if (logger.isTraceEnabled())
        logger.trace(msg)
    }

    def debugIO(msg: => String): UIO[Unit] = ZIO.succeed {
      if (logger.isDebugEnabled)
        logger.debug(msg)
    }

    def infoIO(msg: => String): UIO[Unit] = ZIO.succeed {
      if (logger.isInfoEnabled())
        logger.info(msg)
    }

    def warnIO(msg: => String): UIO[Unit] = ZIO.succeed {
      if (logger.isWarnEnabled())
        logger.warn(msg)
    }

    def errorIO(msg: => String): UIO[Unit] = ZIO.succeed {
      if (logger.isErrorEnabled())
        logger.error(msg)
    }

    def traceIO(msg: => String, th: Throwable): UIO[Unit] = ZIO.succeed {
      if (logger.isTraceEnabled())
        logger.trace(msg, th)
    }

    def debugIO(msg: => String, th: Throwable): UIO[Unit] = ZIO.succeed {
      if (logger.isDebugEnabled)
        logger.debug(msg, th)
    }

    def infoIO(msg: => String, th: Throwable): UIO[Unit] = ZIO.succeed {
      if (logger.isInfoEnabled)
        logger.info(msg, th)
    }

    def warnIO(msg: => String, th: Throwable): UIO[Unit] = ZIO.succeed {
      if (logger.isWarnEnabled)
        logger.warn(msg, th)
    }

    def errorIO(msg: => String, th: Throwable): UIO[Unit] = ZIO.succeed {
      if (logger.isErrorEnabled)
        logger.error(msg, th)
    }

    def traceIO(marker: Marker, msg: => String): UIO[Unit] = ZIO.succeed {
      if (logger.isTraceEnabled(marker))
        logger.trace(marker, msg)
    }

    def debugIO(marker: Marker, msg: => String): UIO[Unit] = ZIO.succeed {
      if (logger.isDebugEnabled(marker))
        logger.debug(marker, msg)
    }

    def infoIO(marker: Marker, msg: => String): UIO[Unit] = ZIO.succeed {
      if (logger.isInfoEnabled(marker))
        logger.info(marker, msg)
    }

    def warnIO(marker: Marker, msg: => String): UIO[Unit] = ZIO.succeed {
      if (logger.isWarnEnabled(marker))
        logger.warn(marker, msg)
    }

    def errorIO(marker: Marker, msg: => String): UIO[Unit] = ZIO.succeed {
      if (logger.isErrorEnabled(marker))
        logger.error(marker, msg)
    }

    def traceIO(marker: Marker, msg: => String, th: Throwable): UIO[Unit] = ZIO.succeed {
      if (logger.isTraceEnabled(marker))
        logger.trace(marker, msg, th)
    }

    def debugIO(marker: Marker, msg: => String, th: Throwable): UIO[Unit] = ZIO.succeed {
      if (logger.isDebugEnabled)
        logger.debug(marker, msg, th)
    }

    def infoIO(marker: Marker, msg: => String, th: Throwable): UIO[Unit] = ZIO.succeed {
      if (logger.isInfoEnabled)
        logger.info(marker, msg, th)
    }

    def warnIO(marker: Marker, msg: => String, th: Throwable): UIO[Unit] = ZIO.succeed {
      if (logger.isWarnEnabled)
        logger.warn(marker, msg, th)
    }

    def errorIO(marker: Marker, msg: => String, th: Throwable): UIO[Unit] = ZIO.succeed {
      if (logger.isErrorEnabled)
        logger.error(marker, msg, th)
    }

    def logIO(msg: LogMessage): UIO[Unit] =
      ZIO.succeed(log(msg))

    def log(msg: LogMessage): Unit =
      if (msg.suppressed) ()
      else
        msg.level match {
          case Level.ERROR => logger.error(msg.text)
          case Level.WARN => logger.warn(msg.text)
          case Level.INFO => logger.info(msg.text)
          case Level.DEBUG => logger.debug(msg.text)
          case Level.TRACE => logger.trace(msg.text)
        }

    def perfLogZIO[R, E, A, E2 >: E, A2 >: A](zio: ZIO[R, E, A])(spec: LogSpec[E2, A2]): ZIO[R, E, A] =
      if (spec.isNoOp) zio
      else
        Clock.nanoTime.flatMap { t0 =>
          def handleError(cause: Cause[E]): ZIO[Any, E, Nothing] =
            if (spec.onError.isEmpty && spec.onTermination.isEmpty) ZIO.failCause(cause)
            else
              Clock.nanoTime.flatMap { t1 =>
                val d = Duration.fromNanos(t1 - t0)

                val msgs = cause.failureOrCause match {
                  case Right(failure) => spec.onTermination.map(_(d, failure))
                  case Left(e) => spec.onError.map(_(d, e))
                }

                ZIO.foreach(msgs)(m => logger.logIO(m)) *> ZIO.failCause(cause)
              }

          def handleSuccess(a: A): ZIO[Any, Nothing, A] =
            if (spec.onSucceed.isEmpty) ZIO.succeed(a)
            else {
              for {
                t1 <- Clock.nanoTime
                d = Duration.fromNanos(t1 - t0)
                msgs = spec.onSucceed.map(_(d, a))
                _ <- ZIO.foreach(msgs)(m => logger.logIO(m))
              } yield a
            }

          zio.foldCauseZIO(handleError, handleSuccess)
        }

    def perfLogIO[R, E, A](zio: ZIO[R, E, A])(spec: LogSpec[E, A]): ZIO[R, E, A] =
      ZIO.environmentWithZIO[R] { r =>
        val io = zio.provideEnvironment(r)
        perfLogZIO(io)(spec).provideLayer(ZLayer.succeed(Clock.ClockLive))
      }

    def perfLog[A](thunk: => A)(spec: LogSpec[Throwable, A]): A =
      if (spec.isNoOp) thunk
      else {
        val t0 = System.nanoTime()
        val res = Try(thunk)

        def onError(th: Throwable): Nothing =
          if (spec.onError.isEmpty) throw th
          else {
            val t1 = System.nanoTime()
            val d = Duration.fromNanos(t1 - t0)
            val msgs = spec.onError.map(_(d, th))
            msgs.foreach(logger.log)
            throw th
          }

        def onSuccess(a: A): A =
          if (spec.onSucceed.isEmpty) a
          else {
            val t1 = System.nanoTime()
            val d = Duration.fromNanos(t1 - t0)
            val msgs = spec.onSucceed.map(_(d, a))
            msgs.foreach(logger.log)
            a
          }

        res match {
          case Failure(e) => onError(e)
          case Success(a) => onSuccess(a)
        }
      }
  }

  object Logging {
    trait Service[-R] {
      final def traceIO(msg: => String): URIO[R, Unit] =
        withUnderlying(_.traceIO(msg))

      final def debugIO(msg: => String): URIO[R, Unit] =
        withUnderlying(_.debugIO(msg))

      final def infoIO(msg: => String): URIO[R, Unit] =
        withUnderlying(_.infoIO(msg))

      final def warnIO(msg: => String): URIO[R, Unit] =
        withUnderlying(_.warnIO(msg))

      final def errorIO(msg: => String): URIO[R, Unit] =
        withUnderlying(_.errorIO(msg))

      final def traceIO(msg: => String, th: Throwable): URIO[R, Unit] =
        withUnderlying(_.traceIO(msg, th))

      final def debugIO(msg: => String, th: Throwable): URIO[R, Unit] =
        withUnderlying(_.debugIO(msg, th))

      final def infoIO(msg: => String, th: Throwable): URIO[R, Unit] =
        withUnderlying(_.infoIO(msg, th))

      final def warnIO(msg: => String, th: Throwable): URIO[R, Unit] =
        withUnderlying(_.warnIO(msg, th))

      final def errorIO(msg: => String, th: Throwable): URIO[R, Unit] =
        withUnderlying(_.errorIO(msg, th))

      final def traceIO(marker: Marker, msg: => String): URIO[R, Unit] =
        withUnderlying(_.traceIO(marker, msg))

      final def debugIO(marker: Marker, msg: => String): URIO[R, Unit] =
        withUnderlying(_.debugIO(marker, msg))

      final def infoIO(marker: Marker, msg: => String): URIO[R, Unit] =
        withUnderlying(_.infoIO(marker, msg))

      final def warnIO(marker: Marker, msg: => String): URIO[R, Unit] =
        withUnderlying(_.warnIO(marker, msg))

      final def errorIO(marker: Marker, msg: => String): URIO[R, Unit] =
        withUnderlying(_.errorIO(marker, msg))

      final def traceIO(marker: Marker, msg: => String, th: Throwable): URIO[R, Unit] =
        withUnderlying(_.traceIO(marker, msg, th))

      final def debugIO(marker: Marker, msg: => String, th: Throwable): URIO[R, Unit] =
        withUnderlying(_.debugIO(marker, msg, th))

      final def infoIO(marker: Marker, msg: => String, th: Throwable): URIO[R, Unit] =
        withUnderlying(_.infoIO(marker, msg, th))

      final def warnIO(marker: Marker, msg: => String, th: Throwable): URIO[R, Unit] =
        withUnderlying(_.warnIO(marker, msg, th))

      final def errorIO(marker: Marker, msg: => String, th: Throwable): URIO[R, Unit] =
        withUnderlying(_.errorIO(marker, msg, th))

      final def logIO(msg: => LogMessage): URIO[R, Unit] =
        withUnderlying(_.logIO(msg))

      final def mdzio: MDZIO = MDZIO

      def logger: URIO[R, Logger]

      private def withUnderlying(op: Logger => UIO[Unit]): URIO[R, Unit] =
        logger.flatMap(op)
    }

    def forClass(clazz: Class[_]): ULayer[Logging] = ZLayer.succeed {
      new Service[Any] {
        @transient
        private lazy val theLogger = getLogger(clazz)

        def logger: UIO[Logger] = ZIO.succeed(theLogger)
      }
    }

    def forLogger(getLogger: => Logger): ULayer[Logging] = ZLayer.succeed {
      new Service[Any] {
        def logger: UIO[Logger] = ZIO.succeed(getLogger)
      }
    }

    def global: ULayer[Logging] = forClass(Logging.getClass)

    val any: ZLayer[Logging, Nothing, Logging] =
      ZLayer.environment[Logging]
  }

  type Logging = Logging.Service[Any]

  val logging: Logging.Service[Logging] = new Logging.Service[Logging] {
    def logger: URIO[Logging, Logger] =
      ZIO.environmentWithZIO[Logging](_.get[Logging.Service[Any]].logger)
  }

  def getLogger[T](implicit classTag: ClassTag[T]): Logger =
    getLogger(classTag.runtimeClass)

  def getLogger(clazz: Class[_]): Logger =
    LoggerFactory.getLogger(clazz)

  def getLogger(name: String): Logger =
    LoggerFactory.getLogger(name)

  def getMarker(name: String): UIO[Marker] =
    ZIO.succeed(MarkerFactory.getMarker(name))

  def makeLogger(name: String): UIO[Logger] =
    ZIO.succeed(getLogger(name))

  def makeLogger[T](implicit classTag: ClassTag[T]): UIO[Logger] =
    ZIO.succeed(getLogger[T])

  def makeLogger[T](clazz: Class[_]): UIO[Logger] =
    ZIO.succeed(getLogger(clazz))

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

  implicit final class ZioLoggingOps[R, E, A](val zio: ZIO[R, E, A]) {
    def perfLogZ[E2 >: E, A2 >: A](spec: LogSpec[E2, A2]): ZIO[R with Logging, E, A] =
      ZIO
        .environmentWithZIO[Logging](_.get[Logging.Service[Any]].logger)
        .flatMap(_.perfLogZIO(zio)(spec))
  }
}
