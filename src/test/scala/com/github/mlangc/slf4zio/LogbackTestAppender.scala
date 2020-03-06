package com.github.mlangc.slf4zio

import java.util.concurrent.atomic.AtomicReference
import java.util.function.UnaryOperator

import ch.qos.logback.classic.spi.ILoggingEvent
import ch.qos.logback.core.AppenderBase
import zio.UIO

class LogbackTestAppender extends AppenderBase[ILoggingEvent] {
  def append(eventObject: ILoggingEvent): Unit = {
    LogbackTestAppender.eventsRef.updateAndGet(new UnaryOperator[List[ILoggingEvent]] {
      def apply(evts: List[ILoggingEvent]): List[ILoggingEvent] = eventObject :: evts
    })

    ()
  }
}

object LogbackTestAppender {
  private val eventsRef = new AtomicReference(List.empty[ILoggingEvent])

  def events: UIO[List[ILoggingEvent]] =
    UIO(eventsRef.get())

  def eventsFor(clazz: Class[_]): UIO[List[ILoggingEvent]] =
    events.map(_.filter(_.getLoggerName == clazz.getCanonicalName))
}

