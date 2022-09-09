package com.github.mlangc.slf4zio.api

import com.github.mlangc.slf4zio.LogbackTestAppender
import com.github.mlangc.slf4zio.LogbackTestUtils
import zio.duration2DurationOps
import zio.durationInt
import zio.test._
import zio.ZIO

object RawPerfLogTest extends ZIOSpecDefault {
  def spec = suite("Raw Performance Log")(
    test("Simple Examples") {
      val spec1 =
        LogSpec.onSucceed[String]((d, s) => info"Good: (${d.render}, $s)") ++
          LogSpec.onError[Throwable]((d, e) => error"Not good: (${d.render}, $e)") ++
          LogSpec.onTermination((d, c) => error"Unexpected: (${d.render}, $c}")

      val spec2 =
        spec1.withThreshold(1.hour)

      val ok = "!!OK!!"
      val error = "!!ERROR!!"

      for {
        logger <- makeLogger(getClass)
        _ <- ZIO.attempt(logger.perfLog(ok)(spec1))
        _ <- ZIO.attempt(logger.perfLog[String](throw new RuntimeException(error))(spec1)).ignore
        _ <- ZIO.attempt(logger.perfLog(ok)(spec2))
        _ <- ZIO.attempt(logger.perfLog[String](throw new RuntimeException(error))(spec2)).ignore
        evts <- LogbackTestAppender.eventsFor(this.getClass)
      } yield assert(evts)(Assertion.hasSize(Assertion.equalTo(2)))
    }
  ) @@ TestAspect.before(LogbackTestUtils.waitForLogbackInitialization.orDie) @@ TestAspect.withLiveClock
}
