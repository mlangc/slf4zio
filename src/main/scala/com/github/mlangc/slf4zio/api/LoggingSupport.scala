package com.github.mlangc.slf4zio.api

import org.slf4j.Logger
import zio.ZIO
import zio.duration.Duration


trait LoggingSupport {
  @transient
  protected final lazy val logger: Logger = getLogger(getClass)

  protected implicit final class ZioLoggerOps[R, E, A](zio: ZIO[R, E, A]) {
    /*
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
    }*/

    def perfLog(spec: LogSpec[E, A]): ZIO[R, E, A] = ???
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
