package com.github.mlangc.slf4zio.api

import com.github.ghik.silencer.silent
import org.slf4j.Logger
import zio.ZIO
import zio.clock.Clock

@silent("inferred to be `Any`")
trait LoggingSupport { outer =>
  @transient
  protected final lazy val logger: Logger = getLogger(getClass)

  protected implicit final class ZioLoggerOps[R, E, A](zio: ZIO[R, E, A]) {
    def perfLog(spec: LogSpec[E, A]): ZIO[R, E, A] =
      ZIO.accessM[R] { r =>
        val io = zio.provide(r)
        io.perfLogZ(spec)
          .provideLayer(Logging.forLogger(logger) ++ Clock.live)
      }
  }

  protected final def perfLog[A](thunk: => A)(spec: LogSpec[Throwable, A]): A =
    logger.perfLog(thunk)(spec)
}
