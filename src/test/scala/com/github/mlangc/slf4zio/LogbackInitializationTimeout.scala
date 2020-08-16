package com.github.mlangc.slf4zio

import zio.duration.Duration
import zio.duration.DurationOps

case class LogbackInitializationTimeout(elapsed: Duration) extends RuntimeException(s"Logback initialization timed out after ${elapsed.render}")
