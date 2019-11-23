package com.github.mlangc.slf4zio.api

import zio.ZIO
import zio.test.Assertion.equalTo
import zio.test.Assertion.isEmpty
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
      val (e, f, g) = ("e", "f", "g")
      val (l, m, n) = ("l", "m", "n")
      val efg = List(e, f, g)
      val lmn = List(l, m, n)

      for {
        v1 <- MDZIO.doWith(e -> l, f -> m, g -> n)(ZIO.foreach(efg)(MDZIO.get))
        v2 <- ZIO.foreach(efg)(MDZIO.get)
      } yield assert(v1.flatten, equalTo(lmn)) && assert(v2.flatten, isEmpty)
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
