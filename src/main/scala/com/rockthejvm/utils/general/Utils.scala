package com.rockthejvm.utils.general

import cats.Functor
import cats.syntax.functor._

extension [F[_], A](fa: F[A])
  def debug(using functor: Functor[F]): F[A] = fa.map { a =>
    val t = Thread.currentThread().getName
    println(s"[$t] $a")
    a
  }