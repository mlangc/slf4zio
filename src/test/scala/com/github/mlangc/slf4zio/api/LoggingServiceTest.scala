package com.github.mlangc.slf4zio.api

import ch.qos.logback.classic.Level
import com.github.mlangc.slf4zio.LogbackTestAppender
import zio.clock.Clock
import zio.duration._
import zio.test.DefaultRunnableSpec
import zio.test._
import zio.test.environment.TestClock

import zio.test.Assertion._

object LoggingServiceTest extends DefaultRunnableSpec {
  def spec = suite("LoggingService")(
    testM("Logging with thresholds") {

      val spec = {
        LogSpec.onError(_ => warn"ERROR") ++
          LogSpec.onSucceed(_ => info"OK") ++
          LogSpec.onTermination(_ => error"UPS")
        }.withThreshold(1.second)

      for {
        _ <- TestClock.adjust(1.milli).perfLogR(spec)
        _ <- TestClock.adjust(1.second + 1.nano).perfLogR(spec)
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
  ).provideSomeLayer[Clock with TestClock](Logging.forClass(getClass))
}
