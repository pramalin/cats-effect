package com.rockthejvm.part3concurrency

import cats.effect.{IO, IOApp}

import java.util.concurrent.Executors
import scala.concurrent.{ExecutionContext, Future}
import scala.util.Try
import com.rockthejvm.utils.*

object AsyncIOs extends IOApp.Simple {

  // IOs can run asynchronously on fibers, without having to manually manage the fiber lifecycle
  val threadPool = Executors.newFixedThreadPool(8)
  val ec: ExecutionContext = ExecutionContext.fromExecutorService(threadPool)
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
   * - lift the Future into IO using Async
   */
  lazy val molFuture: Future[Int] = Future {computeMeaingOfLife()} (ec)

  val ioFuture = {
    IO.async_ { cb =>
      val result = Try(ec.execute { () =>
        molFuture
      }).toEither
      cb(result)
    }
  }


  override def run: IO[Unit] = asyncMolIO_v2.debug >> IO(threadPool.shutdown())
}
