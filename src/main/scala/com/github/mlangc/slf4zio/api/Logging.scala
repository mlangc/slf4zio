package com.github.mlangc.slf4zio.api

import org.slf4j.Logger
import zio.UIO
import zio.URIO

trait Logging {
  def logging: Logging.Service[Any]
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

    def logger: URIO[R, Logger]

    private def withUnderlying(op: Logger => UIO[Unit]): URIO[R, Unit] =
      logger >>= op
  }

  trait ForClass extends Logging {
    protected def clazz: Class[_]

    val logging: Service[Any] = new Service[Any] with Serializable {
      @transient
      private lazy val theLogger = getLogger(clazz)

      def logger: UIO[Logger] = UIO(theLogger)
    }
  }

  object Global extends ForClass {
    protected def clazz: Class[_] = getClass
  }
}

