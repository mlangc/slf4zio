package com.github.mlangc.slf4zio.api

import org.slf4j.event.Level

case class LogMessage(text: String, level: Level, suppressed: Boolean = false)

object LogMessage {
  val Suppressed = LogMessage("", Level.TRACE, true)

  def trace(text: String) = LogMessage(text, Level.TRACE)
  def debug(text: String) = LogMessage(text, Level.DEBUG)
  def info(text: String) = LogMessage(text, Level.INFO)
  def warn(text: String) = LogMessage(text, Level.WARN)
  def error(text: String) = LogMessage(text, Level.ERROR)
}
