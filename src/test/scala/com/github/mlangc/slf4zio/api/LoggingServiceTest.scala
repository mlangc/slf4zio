package com.github.mlangc.slf4zio.api

import ch.qos.logback.classic.Level
import com.github.mlangc.slf4zio.LogbackTestAppender
import com.github.mlangc.slf4zio.LogbackTestUtils
import zio.ZIO
import zio.duration._
import zio.test.Assertion._
import zio.test.DefaultRunnableSpec
import zio.test._
import zio.test.environment.TestClock

object LoggingServiceTest extends DefaultRunnableSpec {
  def spec = suite("LoggingService")(
    testM("Logging with thresholds") {
      val spec = {
        LogSpec.onError(_ => warn"ERROR") ++
          LogSpec.onSucceed(_ => info"OK") ++
          LogSpec.onTermination(_ => error"UPS")
        }.withThreshold(1.second)

      def consume(d: Duration) =
        TestClock.adjust(d) *> ZIO.sleep(d)

      def consumeAndFail(d: Duration) =
        consume(d) *> ZIO.fail(new RuntimeException("Test"))

      def consumeAndDie(d: Duration) =
        consume(d) *> ZIO.dieMessage("Test")

      for {
        _ <- consume(1.milli).perfLogZ(spec)
        _ <- consume(1.second).perfLogZ(spec)
        _ <- consumeAndFail(999.millis + 999.micros + 999.nanos).perfLogZ(spec).ignore
        _ <- consumeAndFail(1.second + 1.nano).perfLogZ(spec).ignore
        _ <- consumeAndDie(Duration.Zero).perfLogZ(spec).catchAllCause(_ => ZIO.unit)
        _ <- consumeAndDie(1.second).perfLogZ(spec).catchAllCause(_ => ZIO.unit)
        evts <- LogbackTestAppender.events.map(evts => evts.filter(evt => evt.getLoggerName == getClass.getCanonicalName))
        warnEvts = evts.filter(_.getLevel == Level.WARN)
        infoEvts = evts.filter(_.getLevel == Level.INFO)
        errEvts = evts.filter(_.getLevel == Level.ERROR)
      } yield {
        assert(warnEvts)(hasSize(equalTo(1))) &&
          assert(infoEvts)(hasSize(equalTo(1))) &&
          assert(errEvts)(hasSize(equalTo(1)))
      }
    }
  ).provideLayer(Logging.forClass(getClass) ++ (environment.Live.default >>> TestClock.default)) @@
    TestAspect.before(LogbackTestUtils.waitForLogbackInitialization.orDie)
}
