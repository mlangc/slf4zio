package com.github.mlangc.slf4zio.api

import zio.ZIO
import zio.test.Assertion.equalTo
import zio.test.Assertion.isNone
import zio.test.DefaultRunnableSpec
import zio.test._

object MDZIOTest extends DefaultRunnableSpec {
  def spec = suite("MDZIO")(
    testM("put, get & clear works properly") {
      val (x, y) = ("x", "y")
      val (a, b) = ("a", "b")

      for {
        _ <- MDZIO.putAll(x -> a, y -> b)
        v1 <- MDZIO.get(x)
        v2 <- MDZIO.get(y)
        _ <- MDZIO.clear()
        v3 <- MDZIO.get(x)
        v4 <- MDZIO.get(y)
      } yield assert((v1, v2, v3, v4), equalTo((Some(a), Some(b), None, None)))
    },
    testM("doWith") {
      val (d, e, f, g) = ("d", "e", "f", "g")
      val (k, l, m, n) = ("k", "l", "m", "n")
      val m0 = "m0"
      val dm0 = List(k, m0)
      val defg = List(d, e, f, g)
      val klmn = List(k, l, m, n)

      for {
        _ <- MDZIO.putAll(d -> k, e -> m0)
        v1 <- MDZIO.doWith(e -> l, f -> m, g -> n)(ZIO.foreach(defg)(MDZIO.get))
        v2 <- ZIO.foreach(defg)(MDZIO.get)
      } yield assert(v1.flatten, equalTo(klmn)) && assert(v2.flatten, equalTo(dm0))
    },
    testM("setContextMap") {
      val (r, s) = ("r", "s")

      for {
        _ <- MDZIO.put(r, s)
        _ <- MDZIO.setContextMap(Map.empty)
        v <- MDZIO.get(r)
      } yield assert(v, isNone)
    }
  )
}
