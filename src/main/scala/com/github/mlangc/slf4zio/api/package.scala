package com.github.mlangc.slf4zio

import org.slf4j.Logger
import org.slf4j.LoggerFactory
import org.slf4j.event.Level
import zio.Cause
import zio.Has
import zio.UIO
import zio.URIO
import zio.ZIO
import zio.ZLayer
import zio.clock
import zio.clock.Clock
import zio.duration.Duration
import zio.scheduler.Scheduler

import scala.reflect.ClassTag
import scala.util.Failure
import scala.util.Success
import scala.util.Try

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

    def perfLogZIO[R, E, A](zio: ZIO[R, E, A])(spec: LogSpec[E, A]): ZIO[R with Clock, E, A] =
      if (spec.isNoOp) zio else clock.nanoTime.flatMap { t0 =>
        def handleError(cause: Cause[E]): ZIO[Clock, E, Nothing] =
          if (spec.onError.isEmpty && spec.onTermination.isEmpty) ZIO.halt(cause)
          else clock.nanoTime.flatMap { t1 =>
            val d = Duration.fromNanos(t1 - t0)

            val msgs = cause.failureOrCause match {
              case Right(failure) => spec.onTermination.map(_ (d, failure))
              case Left(e) => spec.onError.map(_ (d, e))
            }

            ZIO.foreach(msgs)(m => logger.logIO(m)) *> ZIO.halt(cause)
          }

        def handleSuccess(a: A): ZIO[Clock, Nothing, A] =
          if (spec.onSucceed.isEmpty) ZIO.succeed(a) else {
            for {
              t1 <- clock.nanoTime
              d = Duration.fromNanos(t1 - t0)
              msgs = spec.onSucceed.map(_ (d, a))
              _ <- ZIO.foreach(msgs)(m => logger.logIO(m))
            } yield a
          }

        zio.foldCauseM(handleError, handleSuccess)
      }

    def perfLogIO[R, E, A](zio: ZIO[R, E, A])(spec: LogSpec[E, A]): ZIO[R, E, A] =
      ZIO.accessM[R] { r =>
        val io = zio.provide(r)
        perfLogZIO(io)(spec).provideLayer(Scheduler.live >>> Clock.live)
      }

    def perfLog[A](thunk: => A)(spec: LogSpec[Throwable, A]): A =
      if (spec.isNoOp) thunk else {
        val t0 = System.nanoTime()
        val res = Try(thunk)

        def onError(th: Throwable): Nothing =
          if (spec.onError.isEmpty) throw th else {
            val t1 = System.nanoTime()
            val d = Duration.fromNanos(t1 - t0)
            val msgs = spec.onError.map(_ (d, th))
            msgs.foreach(logger.log)
            throw th
          }

        def onSuccess(a: A): A =
          if (spec.onSucceed.isEmpty) a else {
            val t1 = System.nanoTime()
            val d = Duration.fromNanos(t1 - t0)
            val msgs = spec.onSucceed.map(_ (d, a))
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

      final def logIO(msg: => LogMessage): URIO[R, Unit] =
        withUnderlying(_.logIO(msg))

      def logger: URIO[R, Logger]

      private def withUnderlying(op: Logger => UIO[Unit]): URIO[R, Unit] =
        logger >>= op
    }

    def forClass(clazz: Class[_]): ZLayer.NoDeps[Nothing, Logging] = ZLayer.succeed {
      new Service[Any] with Serializable {
        @transient
        private lazy val theLogger = getLogger(clazz)

        def logger: UIO[Logger] = UIO(theLogger)
      }
    }

    def forLogger(getLogger: => Logger): ZLayer.NoDeps[Nothing, Logging] = ZLayer.succeed {
      new Service[Any] with Serializable {
        def logger: UIO[Logger] = UIO(getLogger)
      }
    }

    def global: ZLayer.NoDeps[Nothing, Logging] = forClass(Logging.getClass)
  }

  type Logging = Has[Logging.Service[Any]]

  val logging: Logging.Service[Logging] = new Logging.Service[Logging] {
    def logger: URIO[Logging, Logger] =
      ZIO.accessM[Logging](_.get[Logging.Service[Any]].logger)
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

  def makeLogger[T](clazz: Class[_]): UIO[Logger] =
    UIO(getLogger(clazz))

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
    def perfLogZ(spec: LogSpec[E, A]): ZIO[R with Logging with Clock, E, A] =
      ZIO.accessM[Logging](_.get[Logging.Service[Any]].logger)
        .flatMap(_.perfLogZIO(zio)(spec))
  }
}
