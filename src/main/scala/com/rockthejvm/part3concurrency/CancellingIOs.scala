package com.rockthejvm.part3concurrency

import cats.effect.{IO, IOApp}
import concurrent.duration.*

object CancellingIOs extends IOApp.Simple {
  import com.rockthejvm.utils._
  /*
    Cancelling IOs
    - fib.cancel
    - IO.race & other APIs
    - manual cancellation
   */
  val chainOfIOs = IO("meaning").debug >> IO.canceled >> IO(42).debug

  // uncancelable
  // example: online store, payment processor
  // payment process must not be canceled
  val specialPaymentSystem = (
      IO("Payment running, don't cancel me...").debug >>
      IO.sleep(1.second) >>
        IO("Payment completed.").debug
    ).onCancel(IO("MEGA CANCEL OF DOOM!").debug.void)

  val calncellationOfDoom = for {
    fib <- specialPaymentSystem.start
    _ <- IO.sleep(500.millis) >> fib.cancel
    _ <- fib.join
  } yield ()

  val atomicPayment = IO.uncancelable(_ => specialPaymentSystem) // masking
  val atomicPayment_v2 = specialPaymentSystem.uncancelable

  val noCalncellationOfDoom = for {
    fib <- atomicPayment.start
    _ <- IO.sleep(500.millis) >> IO("attempting cancellation...").debug >> fib.cancel
    _ <- fib.join
  } yield ()

  /*
    The uncancelable API is more complex and more general.
    It takes a function from Poll[IO] to IO. In the example above, we aren't using that Poll instance.
    The Poll object can be used to mark sections within the return effect which can be CANCELLED.
   */

  /*
    Example: authentication service. Has two parts:
    - input password, can be canceled, because otherwise we might block indefinitely on user input
    - verify password, CANNOT be canceled once it started
   */
  val inputPassword = IO("Input password:").debug >> IO("typing password").debug >> IO.sleep(5.seconds) >> IO ("RockTheJVM1!")
  val verifyPassword = (pw: String) => IO("verifying...").debug >> IO.sleep(2.seconds) >> IO(pw == "RockTheJVM1!")

  val authFlow: IO[Unit] = IO.uncancelable { poll =>
    for {
      pw <- poll(inputPassword).onCancel(IO("Authentication timed out. Try again later.").debug.void) // poll makes this cancelable
      verified <- verifyPassword(pw)
      _ <- if (verified) IO("Authentication successful.").debug
      else IO("Authentication failed.").debug
    } yield ()
  }

  val authProgram = for {
    authFib <- authFlow.start
    _ <- IO.sleep(3.seconds) >> IO("Authentication timeout, attempting cancel...").debug >> authFib.cancel
    _ <- authFib.join
  } yield ()

  /*
    Uncancelable calls are MASKS which suppress cancellation.
    Poll calls are "gaps opened" in the uncancellable region.
   */
  override def run: IO[Unit] = authProgram
}
