package com.rockthejvm.part4coordination

import cats.effect.{IO, IOApp}
import cats.effect.std.Semaphore

import scala.concurrent.duration.*
import com.rockthejvm.utils.*
import cats.syntax.parallel.*

import scala.util.Random

object Semaphores extends IOApp.Simple {

  val semaphore: IO[Semaphore[IO]] = Semaphore[IO](2) // 2 total permits

  // example: limiting the number of concurrent sessions on server
  def doWorkWhileLoggedIn(): IO[Int] = IO.sleep(1.second) >> IO(Random.nextInt(100))

  def login(id: Int, sem: Semaphore[IO]): IO[Int] = for {
    _ <- IO(s"[session $id] waiting to log in...").debug
    _ <- sem.acquire
    // critical section
    _ <- IO(s"[session $id logged in, working...").debug
    res <- doWorkWhileLoggedIn()
    _ <- IO(s";session $id] done: $res, logging out...").debug
    // end critical section
    _ <- sem.release
  } yield res

  def demoSemaphore() = for {
    sem <- Semaphore[IO](2)
    user1Fib <- login(1, sem).start
    user2Fib <- login(2, sem).start
    user3Fib <- login(3, sem).start
    _ <- user1Fib.join
    _ <- user2Fib.join
    _ <- user3Fib.join
  } yield ()

  def weightedLogin(id: Int, requiredPermits: Int, sem: Semaphore[IO]) = for {
    _ <- IO(s"[session $id] waiting to log in...").debug
    _ <- sem.acquireN(requiredPermits)
    // critical section
    _ <- IO(s"[session $id logged in, working...").debug
    res <- doWorkWhileLoggedIn()
    _ <- IO(s";session $id] done: $res, logging out...").debug
    // end critical section
    _ <- sem.releaseN(requiredPermits)
  } yield res

  def demoWeightedSemaphore() = for {
    sem <- Semaphore[IO](2)
    user1Fib <- weightedLogin(1, 1, sem).start
    user2Fib <- weightedLogin(2, 2, sem).start
    user3Fib <- weightedLogin(3, 3, sem).start
    _ <- user1Fib.join
    _ <- user2Fib.join
    _ <- user3Fib.join
  } yield ()

  /**
   * Exercise
   * 1. find out if there is something wrong with this code
   * 2. try
   * 3. fix it
   */
  // semaphore with 1 permit == mutex
  val mutex = Semaphore[IO](1)
  val users: IO[List[Int]] = (1 to 10).toList.parTraverse { id =>
    for {
      sem <- mutex
      _ <- IO(s"[session $id] waiting to log in...").debug
      _ <- sem.acquire
      // critical section
      _ <- IO(s"[session $id logged in, working...").debug
      res <- doWorkWhileLoggedIn()
      _ <- IO(s";session $id] done: $res, logging out...").debug
      // end critical section
      _ <- sem.release
    } yield res
  }

  val usersFixed: IO[List[Int]] = mutex.flatMap { sem =>
    (1 to 10).toList.parTraverse { id =>
      for {
        _ <- IO(s"[session $id] waiting to log in...").debug
        _ <- sem.acquire
        // critical section
        _ <- IO(s"[session $id logged in, working...").debug
        res <- doWorkWhileLoggedIn()
        _ <- IO(s";session $id] done: $res, logging out...").debug
        // end critical section
        _ <- sem.release
      } yield res
    }
  }

  override def run: IO[Unit] = usersFixed.debug.void
}
