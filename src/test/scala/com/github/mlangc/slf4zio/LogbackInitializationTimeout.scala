package com.github.mlangc.slf4zio

import zio.Duration
import zio.duration2DurationOps

case class LogbackInitializationTimeout(elapsed: Duration) extends RuntimeException(s"Logback initialization timed out after ${elapsed.render}")
