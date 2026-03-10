package com.rockthejvm.part4coordination

import cats.effect.kernel.Outcome.{Canceled, Errored, Succeeded}
import cats.effect.kernel.{Fiber, Outcome}
import cats.effect.{Deferred, IO, IOApp, Ref}
import com.rockthejvm.utils.*

import scala.concurrent.duration.*
import cats.syntax.traverse.*

object Defers extends IOApp.Simple {

  // defers is a primitive for waiting for an effect, while some other effect completes with a value

  val aDeferred: IO[Deferred[IO, Int]] = Deferred[IO, Int]
  val aDeferred_v2: IO[Deferred[IO, Int]] = IO.deferred[Int] // same

  // get blocks the calling fiber (semantically) until some other fiber completed the Deferred with a value
  val reader: IO[Int] = aDeferred.flatMap { signal =>
    signal.get // Blocks the fiber
  }

  val writer = aDeferred.flatMap{signal =>
    signal.complete(42)
  }

  def demoDeferred(): IO[Unit] = {
    def consumer(signal: Deferred[IO, Int]) = for {
      _ <- IO("[consumer] waiting for result...").debug
      meaningOfLife <- signal.get // blocker
      _ <- IO(s"[consumer] got the result: $meaningOfLife").debug
    } yield ()

    def producer(signal: Deferred[IO, Int]): IO[Unit] = for {
      _ <- IO("[producer] crunching numbers...").debug
      _ <- IO.sleep(1.second)
      _ <- IO("[producer] complete: 42").debug
      meaningOfLife <- IO(42)
      _ <- signal.complete(meaningOfLife)
    } yield ()

    for {
      signal <- Deferred[IO, Int]
      fibConsumer <- consumer(signal).start
      fibProducer <- producer(signal).start
      _ <- fibProducer.join
      _ <- fibConsumer.join
    } yield ()
  }

  // simulate downloading some content
  val fileParts = List("I ", "love S", "cala", " with Cat", "s Effect!<EOF>")

  def fileNotifierWithRef(): IO[Unit] = {
    def downloadFile(contentRef: Ref[IO, String]): IO[Unit] =
      fileParts.map { part =>
        IO(s"[downloader] got $part").debug >> IO.sleep(1.second) >> contentRef.update(currentContent => currentContent + part)
      }
      .sequence
      .void

    def notifyFileComplete(contentRef: Ref[IO, String]): IO[Unit] = for {
      file <- contentRef.get
      _ <- if (file.endsWith("<EOF>")) IO("[notifier] File download complete").debug
      else IO("[notifier] downloading...").debug >> IO.sleep(500.millis) >> notifyFileComplete(contentRef) // busy wait
    } yield ()

    for {
      contentRef <- Ref[IO].of("")
      fibDownloader <- downloadFile(contentRef).start
      notifier <- notifyFileComplete(contentRef).start
      _ <- fibDownloader.join
      _ <- notifier.join
    } yield ()
  }

  // deferred works miracles for waiting
  def fileNotifierWithDeferred(): IO[Unit] = {
    def notifyFileComplete(signal: Deferred[IO, String]): IO[Unit] = for {
      _ <- IO("[notifier] downloading...").debug
      _ <- signal.get // blocks until the signal is completed
      _ <- IO("[notifier] file download complete").debug
    } yield ()

    def downloadFilePart(part: String, contentRef: Ref[IO, String], signal: Deferred[IO, String]): IO[Unit] = for {
      _ <- IO(s"[downloader] got $part").debug
      _ <- IO.sleep(1.second)
      latestContent <- contentRef.updateAndGet(currentContent => currentContent + part)
      _ <- if (latestContent.contains("<EOF")) signal.complete(latestContent) else IO.unit
    } yield ()

    for {
      contentRef <- Ref[IO].of("")
      signal <- Deferred[IO, String]
      notifierFib <- notifyFileComplete(signal).start
      fileTasksFib <- fileParts.map(part => downloadFilePart(part, contentRef, signal)).sequence.start
      - <- notifierFib.join
      _ <- fileTasksFib.join
    } yield ()
  }

  /**
   * Exercises
   * = (medium) write a small alarm notification with two simultaneous IOs
   *   - one that increments a counter every second (a clock)
   *   - one that waits for the counter to become 10, then prints a message "time's up!"
   *
   * - (mega hard) implement racePair with Deferred.
   *   - use a Deferred which can hold on Either[Outcome for ioa, outcome for iob]
   *   - start two fibers, one for each IO
   *   - on completion (with any status), each IO needs to complete that Deferred
   *     (hint: use a finalizer from the Resources lesson)
   *     (hint2: use a guarantee call to make sure the fibers complete the Deferred)
   *   - what do you do in case of cancellation (the hardest part)?
   */
  // 1
  def alarmNotification() : IO[Unit] = {
    def clockUp(countRef: Ref[IO, Int]): IO[Unit] =
      IO(s"[clock] counting...").debug >> IO.sleep(1.second) >> countRef.update(count => count + 1) >> clockUp(countRef)

    def notifyTimeup(countRef: Ref[IO, Int]): IO[Unit] = for {
      count <- countRef.get
      _ <- if (count >= 10) IO("[notifier] time's up").debug
           else notifyTimeup(countRef)
    } yield ()

    for {
      clockRef <- Ref[IO].of(0)
      notifier <- notifyTimeup(clockRef).start
      clock <- clockUp(clockRef).start
      _ <- notifier.join
      _ <- clock.join
    } yield ()
  }

  // 2
  type RaceResult[A, B] = Either[
    (Outcome[IO, Throwable, A], Fiber[IO, Throwable, B]), // (winner result, looser fiber)
    (Fiber[IO, Throwable, A], Outcome[IO, Throwable, B]) // (looser fiber, winner result)
  ]
/* my incomplete answer
  def ourRacer[A, B](ioa: IO[A], iob: IO[B]): IO[RaceResult[A, B]] = for {
    fibA <- ioa.start
    fibB <- iob.start
    signalA <- IO.deferred[A]
    signalB <- IO.deferred[B]
    outA <- fibA.join.flatMap {
      case Succeeded(resultEffect) => resultEffect.map(result => Left(result))
      case Errored(e) => IO.raiseError(e)
      case Canceled() => IO.raiseError(new RuntimeException("Loser canceled"))
    }
    outB <- fibB.join.flatMap {
      case Succeeded(resultEffect) => resultEffect.map(result => Left(result))
      case Errored(e) => IO.raiseError(e)
      case Canceled() => IO.raiseError(new RuntimeException("Loser canceled"))
    }

  } yield (outA, outB)
*/

  // 1
  def eggBoiler(): IO[Unit] = {
    def eggReadyNotification(signal: Deferred[IO, Unit]) = for {
      _ <- IO("Egg boiling on some other fiber, waiting...").debug
      _ <- signal.get
      _ <- IO("EGG READY").debug
    } yield ()

    def tickingClock(ticks: Ref[IO, Int], signal: Deferred[IO, Unit]): IO[Unit] = for {
      _ <- IO.sleep(1.second)
      count <- ticks.updateAndGet(_ + 1)
      _ <- IO(count).debug
      _ <- if (count >= 10) signal.complete(()) else tickingClock(ticks, signal)
    } yield ()

    for {
      counter <- Ref[IO].of(0)
      signal <- Deferred[IO, Unit]
      notificationFib <- eggReadyNotification(signal).start
      clock <- tickingClock(counter, signal).start
      _ <- notificationFib.join
      _ <- clock.join
    } yield ()
  }

  // 2
  type EitherOutcome[A, B] = Either[Outcome[IO, Throwable, A], Outcome[IO, Throwable, B]]
  def ourRacer[A, B](ioa: IO[A], iob: IO[B]): IO[RaceResult[A, B]] = IO.uncancelable { poll =>
    for {
      signal <- Deferred[IO, EitherOutcome[A, B]]
      fibA <- ioa.guaranteeCase(outcomeA => signal.complete(Left(outcomeA)).void).start
      fibB <- iob.guaranteeCase(outcomeB => signal.complete(Right(outcomeB)).void).start
      result <- poll(signal.get).onCancel { // blocking call - should be cancelable
          for {
            cancelFibA <- fibA.cancel.start
            cancelFibB <- fibB.cancel.start
            _ <- cancelFibA.join
            _ <- cancelFibB.join
          } yield ()
        }
      } yield result match {
      case Left(outcomeA) => Left((outcomeA, fibB))
      case Right(outcomeB) => Right((fibA, outcomeB))
    }
  }

  IO.racePair()

  override def run: IO[Unit] = eggBoiler()
}
