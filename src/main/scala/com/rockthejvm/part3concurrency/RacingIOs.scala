package com.rockthejvm.part3concurrency

import cats.effect.kernel.Outcome
import cats.effect.{Fiber, IO, IOApp}

import concurrent.duration.FiniteDuration
import concurrent.duration.*

object RacingIOs extends IOApp.Simple {

  import com.rockthejvm.utils._

  def runWithSleep[A](value: A, duration: FiniteDuration): IO[A] =
    (
      IO(s"starting computation: $value").debug >>
      IO.sleep(duration) >>
      IO(s"computation for $value done") >>
      IO(value)
    ).onCancel(IO(s"computation CANCELED for $value").debug.void)

  def testRace() = {
    val meaningOfLife = runWithSleep(42, 1.second)
    val favLang = runWithSleep("Scala", 2.seconds)
    val first: IO[Either[Int, String]] = IO.race(meaningOfLife, favLang)
    /*
      - both IOs run on separate fibers
      - the first one to finish will complete the result
      - the loser will be canceled
     */

    first.flatMap {
      case Left(mol) => IO(s"Meaning of life won: $mol")
      case Right(lang) => IO(s"Fav language won: $lang")
    }
  }

  def testRacePair() = {
    val meaningOfLife = runWithSleep(42, 1.second)
    val favLang = runWithSleep("Scala", 2.seconds)
    val raceResult: IO[Either[
      (Outcome[IO, Throwable, Int], Fiber[IO, Throwable, String]), // (winner result, looser fiber)
      (Fiber[IO, Throwable, Int], Outcome[IO, Throwable, String])  // (looser fiber, winner result)
    ]] = IO.racePair(meaningOfLife, favLang)

    raceResult.flatMap {
      case Left(outMol, fibLang) => fibLang.cancel >> IO("MOL won").debug >> IO(outMol).debug
      case Right(fibMol, outLang) => fibMol.cancel >> IO("Language won").debug >> IO(outLang).debug
    }
  }

  /**
   * Exercises:
   * 1 - implement a timeout pattern with race
   * 2 - a method to return LOSING effect from a race (hint: use racePair)
   * 3 - implement race in terms of racePair
   */
  // 1
  def timeout[A](io: IO[A], duration: FiniteDuration): IO[A] = {
      val timeoutIO = IO("Cancelling").debug >> IO.sleep(duration)
      val raceResult = IO.race(timeoutIO, io)

      raceResult.flatMap {
        case Left(()) => IO.raiseError(new RuntimeException("timedout"))
        case Right(a) => IO(a)
      }
  }

  // 2
  def unrace[A, B](ioa: IO[A], iob: IO[B]): IO[Either[A, B]] = {
    val failed = IO.racePair(ioa, iob).flatMap {
      case Left((outA, fibB)) => IO(Right(fibB))
      case Right((fibA, outB)) => IO(Left(fibA))
    }

    val result = failed.map((aOrb => aOrb.flatMap {
            case Left(a: Fiber[IO, Throwable, A]) => a
            case Right(b: Fiber[IO, Throwable, B]) => b
          }
        )
  }

  // 3
  def simpleRace[A, B](ioa: IO[A], iob: IO[B]): IO[Either[A, B]] = {
    IO.racePair(ioa, iob).flatMap {
      case Left((outA, fibB)) => IO(Left(outA))
      case Right((fibA, outB)) => IO(Right(outB))
    }
  }

  override def run: IO[Unit] = testRacePair().void
}
