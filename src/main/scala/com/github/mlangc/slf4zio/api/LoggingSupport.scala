package com.github.mlangc.slf4zio.api

import com.github.ghik.silencer.silent
import org.slf4j.Logger
import zio.ZIO

@silent("inferred to be `Any`")
trait LoggingSupport { outer =>
  @transient
  protected final lazy val logger: Logger = getLogger(getClass)

  protected implicit final class ZioLoggerOps[R, E, A](zio: ZIO[R, E, A]) {
    def perfLog[E1 >: E](spec: LogSpec[E1, A]): ZIO[R, E, A] =
      ZIO.environmentWithZIO[R] { r =>
        val io = zio.provideEnvironment(r)
        io.perfLogZ(spec)
          .provideLayer(Logging.forLogger(logger))
      }
  }

  protected final def perfLog[A](thunk: => A)(spec: LogSpec[Throwable, A]): A =
    logger.perfLog(thunk)(spec)
}
