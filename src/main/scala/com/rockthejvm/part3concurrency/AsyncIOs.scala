package com.rockthejvm.part3concurrency

import cats.effect.{IO, IOApp}

import java.util.concurrent.Executors
import scala.concurrent.{ExecutionContext, Future}
import scala.util.Try
import com.rockthejvm.utils.*
import scala.concurrent.duration._

object AsyncIOs extends IOApp.Simple {

  // IOs can run asynchronously on fibers, without having to manually manage the fiber lifecycle
  val threadPool = Executors.newFixedThreadPool(8)
  given ec: ExecutionContext = ExecutionContext.fromExecutorService(threadPool)
  type Callback[A] = Either[Throwable, A] => Unit


  def computeMeaingOfLife(): Int = {
    Thread.sleep(1000)
    println(s"${Thread.currentThread().getName}] computing the meaning of life on another thread...")
    42
  }

  def computeMeaingOfLifeEither(): Either[Throwable, Int] = Try {
    computeMeaingOfLife()
  }.toEither

  def computeMolThreadPool(): Unit =
    threadPool.execute(() => computeMeaingOfLife())

  // lift computation to an IO
  // async is a FFI
  val asyncMolIO: IO[Int] = IO.async_ { cb => // CE thread blocks (semantically) until this cb is invoked (by some other thread)
    threadPool.execute { () => // computation not managed by CE
      val result = computeMeaingOfLifeEither()
      cb(result) // CE thread is notified with the result
    }
  }

  /**
   * Exercise
   */
  def myAsyncToIO[A](computation: () => A)(ec: ExecutionContext): IO[A] =
    IO.async_ { cb =>
      ec.execute { () => {
        val result: Either[Throwable, A] = Try {
          computation()
          }.toEither

          cb(result)
        }
      }
    }

  def asyncToIO[A](computation: () => A)(ec: ExecutionContext): IO[A] =
    IO.async_ {(cb: Callback[A]) =>
      ec.execute { () =>
        val result = Try(computation()).toEither
        cb(result)
      }
    }

  val asyncMolIO_v2 = asyncToIO(computeMeaingOfLife)(ec)

  /*
   * Exercise
   * - lift an async computationas a Future, to an IO.
   */

  // my answer
  val ioFuture = {
    IO.async_ { cb =>
      val result = Try(ec.execute { () =>
        molFuture
      }).toEither
      cb(result)
    }
  }

  def convertFutureToIO[A](future: => Future[A]): IO[A] =
    IO.async_ { (cb: Callback[A]) =>
      future.onComplete { tryResult =>
        val result = tryResult.toEither
        cb(result)
      }
    }

  lazy val molFuture: Future[Int] = Future {computeMeaingOfLife()}
  val asyncMolIO_v3: IO[Int] = convertFutureToIO(molFuture)
  val asyncMolIO_v4: IO[Int] = IO.fromFuture(IO(molFuture)) // CE API

  /**
   * Exercise: a never-ending IO?
   */
  val neverEndingIO: IO[Int] = IO.async_[Int](_ => ()) // no callback, no finish
  val neverEndingIO_v2: IO[Int] = IO.never

  /*
    FULL ASYNC call
   */
  def demoAsyncCancellation() = {
    val asyncMeaningOfLifeIO_v2: IO[Int] = IO.async {(cb: Callback[Int]) =>
      /*
        finalizer in case of computation get canceled.
        finalizers are of type IO[Unit]
        not specifying finalizer => Option[IO[Unit]]
        creating option is an effect => IO[Option[Option[IO[Unit]]]]
       */
      // return IO[Option[IO[Unit]]]
      IO {
        threadPool.execute { () =>
          val result = computeMeaingOfLifeEither()
          cb(result)
        }
      }.as(Some(IO("Cancelled!").debug.void))
    }

    for {
      fib <- asyncMeaningOfLifeIO_v2.start
      _ <- IO.sleep(500.millis) >> IO("cancelling...").debug >> fib.cancel
      _ <- fib.join
    } yield ()

  }

  override def run: IO[Unit] = demoAsyncCancellation().debug >> IO(threadPool.shutdown())
}
