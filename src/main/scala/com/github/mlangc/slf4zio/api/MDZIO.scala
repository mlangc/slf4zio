package com.github.mlangc.slf4zio.api

import org.slf4j.MDC
import zio.UIO
import zio.ZIO

import scala.collection.JavaConverters._

/**
 * Convenience APIs for interacting with the MDC context.
 *
 * ==Important==
 * Make sure that you use a fiber aware MDC implementation, as provided
 * for example by <a href="https://github.com/mlangc/zio-interop-log4j2">zio-interop-log4j2</a>.
 * Using the convenience wrappers found here won't make the underlying MDC context implementation
 * aware of ZIO fibers.
 */
object MDZIO {
  def put(key: String, value: String): UIO[Unit] =
    UIO(MDC.put(key, value))

  def get(key: String): UIO[Option[String]] =
    UIO(Option(MDC.get(key)))

  def remove(key: String): UIO[Unit] =
    UIO(MDC.remove(key))

  def clear(): UIO[Unit] =
    UIO(MDC.clear())

  def putAll(pairs: (String, String)*): UIO[Unit] =
    putAll(pairs)

  def putAll(pairs: Iterable[(String, String)]): UIO[Unit] =
    ZIO.foreach_(pairs)((put _).tupled)

  def removeAll(keys: Iterable[String]): UIO[Unit] =
    ZIO.foreach_(keys)(remove)

  /**
   * Puts the given key value pairs in the context, executes the given action, and restores the original context.
   */
  def doWith[R, E, A](pairs: Iterable[(String, String)])(zio: ZIO[R, E, A]): ZIO[R, E, A] =
    for {
      state1 <- ZIO.succeed(pairs.toMap)
      state0 <- getAll(state1.keys)
      newKeys = state1.keySet.diff(state0.keySet)
      a <- (putAll(state1) *> zio).ensuring(removeAll(newKeys) *> putAll(state0))
    } yield a


  def doWith[R, E, A](pairs: (String, String)*)(zio: ZIO[R, E, A]): ZIO[R, E, A] =
    doWith(pairs)(zio)

  def getContextMap: UIO[Option[Map[String, String]]] =
    UIO(Option(MDC.getCopyOfContextMap).map(_.asScala.toMap))

  def setContextMap(map: Map[String, String]): UIO[Unit] =
    UIO(MDC.setContextMap(map.asJava))

  private def getAll(keys: Iterable[String]): UIO[Map[String, String]] =
    ZIO.foldLeft(keys)(Map.empty[String, String]) { (acc, key) =>
      get(key).map(_.fold(acc)(v => acc + (key -> v)))
    }
}
