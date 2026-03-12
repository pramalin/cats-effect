package com.rockthejvm.part5polymorphic

import cats.effect.{Concurrent, IO, IOApp, Temporal}

import scala.concurrent.duration._
import com.rockthejvm.utils.general._
import cats.syntax.functor._
import cats.syntax.flatMap._

object PolymorphicTemporalSuspension extends IOApp.Simple {

  // Temporal - time-blocking effects
  trait MyTemporal[F[_]] extends Concurrent[F] {
    def sleep(time: FiniteDuration): F[Unit] // semantically blocks this fiber for a specified time
  }

  // abilities: pure, map/flatMap, raiseError, uncancellable, start, ref/deferred, +sleep
  val temporalIO = Temporal[IO] // given Temporal[IO] in scope
  val chainOfEffects = IO("Loading...").debug *> IO.sleep(1.second) *> IO("Game ready!").debug
  val chainOfEffects_v2 = temporalIO.pure("Loading...").debug *> temporalIO.sleep(1.second) *> temporalIO.pure("Game ready!").debug


  /**
   * Exercise:
   * 1 - timeout - generalize
   */
  def timeout[F[_], A](fa: F[A], duration: FiniteDuration)(using temporal: Temporal[F]): F[A] = {
    val timeoutIO = temporal.pure("Cancelling").debug >> temporal.sleep(duration)
    val raceResult = temporal.race(timeoutIO, fa)

    raceResult.flatMap {
      case Left(()) => temporal.raiseError(new RuntimeException("timedout"))
      case Right(a) => temporal.pure(a)
    }
  }


  override def run: IO[Unit] = ???
}
