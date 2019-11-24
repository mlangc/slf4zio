package com.github.mlangc.slf4zio.api

import com.github.ghik.silencer.silent
import org.slf4j.Logger
import zio.ZIO
import zio.clock.Clock
import zio.duration.Duration
import zio.scheduler.Scheduler

import scala.util.Try


@silent("inferred to be `Any`")
trait LoggingSupport { outer =>
  @transient
  protected final lazy val logger: Logger = getLogger(getClass)

  protected implicit final class ZioLoggerOps[R, E, A](zio: ZIO[R, E, A]) {
    def perfLog(spec: LogSpec[E, A]): ZIO[R, E, A] =
      ZIO.accessM[R] { r =>
        val io = zio.provide(r)
        io.perfLogR(spec)
          .provideLayer(Logging.forLogger(logger) ++ (Scheduler.live >>> Clock.live))
      }
  }

  protected final def perfLog[A](spec: LogSpec[Throwable, A])(thunk: => A): A =
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

      res.fold(onError, onSuccess)
    }
}
