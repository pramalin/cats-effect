package com.rockthejvm.part3consurrency

import cats.effect.kernel.Outcome.{Canceled, Errored, Succeeded, errored}
import cats.effect.{Fiber, IO, IOApp, Outcome}

import concurrent.duration.{DurationInt, FiniteDuration}

object Fibers extends IOApp.Simple {

  val meaningOfLife = IO.pure(42)
  val favLang = IO.pure("Scala")

  import com.rockthejvm.utils._

  def sameThredIOs() = for {
    _ <- meaningOfLife.debug
    _ <- favLang.debug
  } yield ()

  // introduce the Fiber
  def createFiber: Fiber[IO, Throwable, String] = ??? // almost impossible to create Fibers manually

  val aFiber: IO[Fiber[IO, Throwable, Int]] = meaningOfLife.debug.start

  def differentThreadIOs() = for {
    _ <- aFiber
    _ <- favLang.debug
  } yield ()

  // joining a fiber
  def runOnSomeOtherThread[A](io: IO[A]): IO[Outcome[IO, Throwable, A]] = for {
    fib <- io.start
    result <- fib.join
  } yield result
  /*
      IO[ResultType of fib.join]
      fib.join = OutCome[IO, Throwable, A]

      possible outcomes:
      - success with an IO
      - failure with an exception
      - cancelled
   */

  val sumIOOnAnotherThread = runOnSomeOtherThread(meaningOfLife)
  val someResultFromAnotherThread = sumIOOnAnotherThread.flatMap {
    case Succeeded(effect) => effect
    case Errored(e) => IO(0)
    case Canceled() => IO(0)
  }

  def throwOnAnotherThread() = for {
    fib <- IO.raiseError[Int](new RuntimeException("no number for you")).start
    result <- fib.join
  } yield result

  def testCancel() = {
    val task = IO("Starting").debug >> IO.sleep(1.second) >> IO("done").debug
    val taskWithCancellationHandler = task.onCancel(IO("I'm being cancelled").debug.void)
    for {
      fib <- taskWithCancellationHandler.start // on a separate thread
      _ <- IO.sleep(500.millis) >> IO("cancelling").debug // running on calling thread
      _ <- fib.cancel
      result <- fib.join
    } yield result
  }

  /**
   * Exercises
   *  1. Write a function that runs on IO on another thread, and, depending on the result of the fiber
   *    - return the result in an IO
   *    - if errored or cancelled, return a failed IO
   *
   * 2. Write a function that takes two IOs, runs on different fibers and returns an IO with a tuple containing both results
   *    - if both IOs complete successfully, tuple their results
   *    - if the first IO returns an error, raise that error (ignoring the second IO's result/error)
   *    - if the first IO doesn't error but second IO returns an error, raise the error
   *    - if one (or both) cancelled, raise a RuntimeException
   *
   * 3. Write a function that adds a timeout to an IO:
   *    - IO runs on a fiber
   *    - if the timeout duration passes, then the fiber is cancelled
   *    - the method returns on IO[A] which contains
   *      - the original value if the computation is successful before the timeout signal
   *      - the exception if the computation is failed before the timeout signal
   *      - a RuntimeException if it times out (i.e. cancelled by the timeout)
   */
  // 1
  def myProcessResultsFromFiber[A](io: IO[A]): IO[A] = {
    val outcome = for {
      fib <- io.start
      result <- fib.join
    } yield result

    outcome.flatMap {
      case Succeeded(a) => a
      case Errored(_) => IO.raiseError(new RuntimeException("errored"))
      case Canceled() => IO.raiseError(new RuntimeException("errored"))
    }
  }

  def processResultsFromFiber[A](io: IO[A]): IO[A] = {
    val ioResult = for {
      fib <- io.debug.start
      result <- fib.join
    } yield result

    ioResult.flatMap {
      case Succeeded(fa) => fa
      case Errored(e) => IO.raiseError(e)
      case Canceled() => IO.raiseError(new RuntimeException("Computation canceled"))
    }
  }

  def testEx1() = {
    val aComputation = IO("starting").debug >> IO.sleep(1.second) >> IO("done").debug >> IO(42)
    processResultsFromFiber(aComputation).void
  }

    // 2
  def myTupleIOs[A, B](ioa: IO[A], iob: IO[B]): IO[(A, B)] = {
      val aOut = for {
        fibA <- ioa.start
        aResult <- fibA.join
      } yield aResult

      val bOut = for {
        fibB <- iob.start
        bResult <- fibB.join
      } yield bResult

      aOut.flatMap {
          case Succeeded(aIO) => bOut.flatMap{
            case Succeeded(bIO) => aIO.flatMap(a => bIO.flatMap(b => IO((a, b))))
            case Errored(e) => IO.raiseError(e)
            case Canceled() => IO.raiseError(new RuntimeException("at least one got cancelled"))
            }
          case Canceled() => bOut.flatMap {
            case Canceled() => IO.raiseError(new RuntimeException("at least one got cancelled"))
          }
          case Errored(e) => IO.raiseError(e)
          case _ => bOut.flatMap {
                  case Errored(e) => IO.raiseError(e)
              }
      }
  }

  def tupleIOs[A, B](ioa: IO[A], iob: IO[B]): IO[(A, B)] = {
    val result = for {
      fiba <- ioa.start
      fibb <- iob.start
      resulta <- fiba.join
      resultb <- fibb.join
    } yield (resulta, resultb)

    result.flatMap {
      case (Succeeded(fa), Succeeded(fb)) => for {
        a <-fa
        b <- fb
      } yield (a, b)
      case (Errored(e), _) => IO.raiseError(e)
      case (_, Errored(e)) => IO.raiseError(e)
      case _  => IO.raiseError(new RuntimeException("Some computation cancelled"))
    }
  }

  def testEx2() = {
    val firstIO = IO.sleep(2.seconds) >> IO(1).debug
    val secondIO = IO.sleep(3.seconds) >> IO(2).debug

    tupleIOs(firstIO, secondIO).debug.void
  }



    // 3
  def myTimeout[A](io: IO[A], duration: FiniteDuration): IO[A] =
    io.timeout(duration)

  def timeout[A](io: IO[A], duration: FiniteDuration): IO[A] = {
    val computation = for {
      fib <- io.start
//      _ <- IO.sleep(duration) >> fib.cancel
      _ <- (IO.sleep(duration) >> fib.cancel).start // alternatively start cancel on a different thread
      result <- fib.join
    } yield result

    computation.flatMap {
      case Succeeded(fa) => fa
      case Errored(e) => IO.raiseError(e)
      case Canceled() => IO.raiseError(new RuntimeException("Computation cancelled"))
    }
  }

  def testEx3() = {
    val aComputation = IO("starting").debug >> IO.sleep(1.second) >> IO("done").debug >> IO(42)
    timeout(aComputation, 500.millis).debug.void
  }

  override def run: IO[Unit] = testEx3()
}