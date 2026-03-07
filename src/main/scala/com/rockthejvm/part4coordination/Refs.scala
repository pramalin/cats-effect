package com.rockthejvm.part4coordination

import cats.effect.{IO, IOApp, Ref}
import com.rockthejvm.utils._
import scala.concurrent.duration._

object Refs extends IOApp.Simple {

  // ref - purely functional atomic reference
  val atomicMol: IO[Ref[IO, Int]] = Ref[IO].of(42)
  val atomicMol_v2: IO[Ref[IO, Int]] = IO.ref(42)

  // modifying is an effect
  val increasedMol: IO[Unit] = atomicMol.flatMap( ref =>
    ref.set(43) // thread-safe
  )

  // obtain a value
  val mol = atomicMol.flatMap { ref =>
    ref.get // thread-safe
  }

  val gsMol: IO[Int] = atomicMol.flatMap { ref =>
    ref.getAndSet(43)
  } // gets the old value, sets the new one

  // updating with function
  val fMol = atomicMol.flatMap { ref =>
    ref.update(value => value * 10)
  }

  val updateMol: IO[Int] = atomicMol.flatMap { ref =>
    ref.updateAndGet(value => value * 10)  // get the new value
    // can also use getAndUpdate to get the OLD value
  }

  val modifyMol: IO[String] = atomicMol.flatMap{ ref =>
    ref.modify(value => (value * 10, s"my current value is $value"))
  }

  // why: concurrent + thread-safe reads/writes over shared value, in a purely functional way

  import cats.syntax.parallel._
  def demoConcurrentWorkImpure(): IO[Unit] = {
    var count = 0

    def task(workload: String): IO[Unit] = {
      val wordCount = workload.split(" ").length
      for {
        _ <- IO(s"Counting words for '$workload': $wordCount").debug
        newCount = count + wordCount
        _ <- IO(s"New total: $newCount").debug
        _ = count = newCount
      } yield ()
    }
    /*
      Drawbacks:
      - hard to read/debug
      - mix pure/impure code
      - NOT THREAD SAFE
     */
    List("a as in apple", "b for biscuit", "c for cat")
      .map(task)
      .parSequence
      .void
  }

  def demoConcurrentWorkPure(): IO[Unit] = {
      def task(workload: String, total: Ref[IO, Int]): IO[Unit] = {
        val wordCount = workload.split(" ").length

        for {
          _ <- IO(s"Counting words for '$workload': $wordCount").debug
          newCount <- total.updateAndGet(currentCount => currentCount + wordCount)
          _ <- IO(s"New total: $newCount").debug
        } yield ()
      }

      for {
        initialCount <- Ref[IO].of(0)
        _ <- List("a as in apple", "b for biscuit", "c for cat")
            .map(string => task(string, initialCount))
            .parSequence
      } yield ()
  }

  /**
   * Exercise
   */
  def tickingClockImpure(): IO[Unit] = {
    var ticks: Long = 0L
    def tickingClock: IO[Unit] = for {
      _ <- IO.sleep(1.second)
      _ <- IO(System.currentTimeMillis()).debug
      _ <- IO(ticks += 1)
      _ <- tickingClock
    } yield ()

    def printTicks: IO[Unit] = for {
      _ <- IO.sleep(5.seconds)
      _ <- IO(s"TICKS: $ticks").debug
      _ <- printTicks
    } yield ()

    for {
      _ <- (tickingClock, printTicks).parTupled
    } yield ()
  }
  /**
   * solution:
   */
  def tickingClockPure(): IO[Unit] = {
    var ticks: IO[Ref[IO, Long]] = Ref[IO].of(0L)

    def tickingClock(ticks: Ref[IO, Int]):IO[Unit] = for {
      _ <- IO.sleep(1.second)
      _ <- IO(System.currentTimeMillis()).debug
      _ <- ticks.update( _ + 1)
      _ <- tickingClock(ticks)
    } yield ()

    def printTicks(ticks:Ref[IO, Int]): IO[Unit] = for {
      _ <- IO.sleep(5.seconds)
      t <- ticks.get
      _ <- IO(s"TICKS: $t").debug
      _ <- printTicks(ticks)
    } yield ()

    for {
      tickRef <- Ref[IO].of(0)
      _ <- (tickingClock(tickRef), printTicks(tickRef)).parTupled
    } yield ()
  }


  def tickingClockWeird(): IO[Unit] = {
    val ticks: IO[Ref[IO, Int]] = Ref[IO].of(0)

    def tickingClock: IO[Unit] = for {
      t <- ticks
      _ <- IO.sleep(1.second)
      _ <- IO(System.currentTimeMillis()).debug
      _ <- t.update(_ + 1)
      _ <- tickingClock
    } yield ()

    def printTicks: IO[Unit] = for {
      t <- ticks
      _ <- IO.sleep(5.seconds)
      currentTicks <- t.get
      _ <- IO(s"TICKS: $currentTicks").debug
      _ <- printTicks
    } yield ()

    for {
      _ <- (tickingClock, printTicks).parTupled
    } yield ()
  }

  override def run: IO[Unit] = tickingClockWeird()
}
