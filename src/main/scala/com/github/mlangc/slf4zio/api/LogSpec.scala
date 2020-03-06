package com.github.mlangc.slf4zio.api

import zio.Cause
import zio.duration.Duration

case class LogSpec[-E, -A](onError: List[(Duration, E) => LogMessage] = Nil,
                           onSucceed: List[(Duration, A) => LogMessage] = Nil,
                           onTermination: List[(Duration, Cause[Nothing]) => LogMessage] = Nil) {

  def combine[E2 <: E, A2 <: A](other: LogSpec[E2, A2]): LogSpec[E2, A2] =
    LogSpec(
      onError = onError ::: other.onError,
      onSucceed = onSucceed ::: other.onSucceed,
      onTermination = onTermination ::: other.onTermination
    )

  def ++[E2 <: E, A2 <: A](other: LogSpec[E2, A2]) = combine(other)

  def withThreshold(threshold: Duration): LogSpec[E, A] = {
    LogSpec(
      onError.map(f => (d: Duration, e: E) => if (d < threshold) LogMessage.Suppressed else f(d, e)),
      onSucceed.map(f => (d: Duration, e: A) => if (d < threshold) LogMessage.Suppressed else f(d, e)),
      onTermination.map(f => (d: Duration, c: Cause[Nothing]) => if (d < threshold) LogMessage.Suppressed else f(d, c))
    )
  }

  def isNoOp: Boolean =
    this == LogSpec.NoOp
}

object LogSpec {
  val NoOp: LogSpec[Any, Any] = LogSpec(Nil, Nil, Nil)

  def onSucceed(msg: Duration => LogMessage): LogSpec[Any, Any] =
    onSucceed((d, _) => msg(d))

  def onSucceed[A](msg: (Duration, A) => LogMessage): LogSpec[Any, A] =
    LogSpec(onSucceed = List(msg))

  def onError[E](msg: (Duration, E) => LogMessage): LogSpec[E, Any] =
    LogSpec(onError = List(msg))

  def onError(msg: Duration => LogMessage): LogSpec[Any, Any] =
    onError((d, _) => msg(d))

  def onTermination(msg: (Duration, Cause[Nothing]) => LogMessage): LogSpec[Any, Any] =
    LogSpec(onTermination = List(msg))

  def onTermination(msg: Duration => LogMessage): LogSpec[Any, Any] =
    onTermination((d, _) => msg(d))

  def onTerminationOrError[E](msg: (Duration, Cause[E]) => LogMessage): LogSpec[E, Any] =
    onError[E]((d, e) => msg(d, Cause.fail(e))) ++ onTermination(msg)

  def onTerminationOrError(msg: Duration => LogMessage): LogSpec[Any, Any] =
    onTerminationOrError((d, _) => msg(d))
}
