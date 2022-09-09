package com.github.mlangc.slf4zio

import zio.duration2DurationOps
import zio.Duration

case class LogbackInitializationTimeout(elapsed: Duration)
    extends RuntimeException(s"Logback initialization timed out after ${elapsed.render}")
