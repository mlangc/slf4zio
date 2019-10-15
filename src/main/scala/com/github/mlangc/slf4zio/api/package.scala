package com.github.mlangc.slf4zio

import org.slf4j.Logger
import org.slf4j.LoggerFactory
import zio.UIO
import zio.URIO
import zio.ZIO

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
  }

  object logging extends Logging.Service[Logging] {
    def logger: URIO[Logging, Logger] =
      ZIO.accessM[Logging](_.logging.logger)
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
}
