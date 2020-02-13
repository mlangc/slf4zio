package com.github.mlangc.slf4zio.api

import zio.Cause
import zio.duration.Duration

case class LogSpec[-E, -A](onError: List[(Duration, E) => String] = Nil,
                           onSucceed: List[(Duration, A) => String] = Nil,
                           onTermination: List[(Duration, Cause[Nothing]) => String] = Nil) {
  def combine[E2, A2]
}

object LogSpec {
  def onSucceed(msg: Duration => String): LogSpec[Any, Any] =
    onSucceed((d, _) => msg(d))

  def onSucceed[A](msg: (Duration, A) => String): LogSpec[Any, A] =
    LogSpec(onSucceed = List(msg))

  def onError[E](msg: (Duration, E) => String): LogSpec[E, Any] =
    LogSpec(onError = List(msg))

  def onError(msg: Duration => String): LogSpec[Any, Any] =
    onError((d, _) => msg(d))

  def onTermination(msg: (Duration, Cause[Nothing]) => String): LogSpec[Any, Any] =
    LogSpec(onTermination = List(msg))

  def onTermination(msg: Duration => String): LogSpec[Any, Any] =
    onTermination((d, _) => msg(d))
}
