package com.rockthejvm.part5polymorphic

import cats.effect.kernel.Outcome.{Canceled, Errored, Succeeded}
import cats.{Applicative, Monad}
import cats.effect.{IO, IOApp, MonadCancel, Poll}
import com.rockthejvm.utils._

object PolymorphicCancellation extends IOApp.Simple {

  trait MyApplicativeError[F[_], E] extends Applicative[F] {
    def raiseError[A](error: E): F[A]
    def handleErrorWith[A](fa: F[A])(f: E => F[A]): F[A]
  }

  trait MyMonadError[F[_], E] extends MyApplicativeError[F, E] with Monad[F]

  // MonadCancel

  trait MyMonadCancel[F[_], E] extends MyMonadError[F, E] {
    def canceled: F[Unit]
    def uncancellable[A](poll: Poll[F] => F[A]): F[A]
  }

  trait MyPoll[F[_]] {
    def apply[A](fa: F[A]): F[A]
  }

  // monadCancel for IO
  val monadCancelIO: MonadCancel[IO, Throwable] = MonadCancel[IO]

  // we can create values
  val molIO: IO[Int] = monadCancelIO.pure(42)
  val ambitiousMolIO: IO[Int] = monadCancelIO.map(molIO)(_ * 10)

  val mustCompute = monadCancelIO.uncancelable { _ =>
    for {
      _ <- monadCancelIO.pure("once started, I can't go back")
      res <- monadCancelIO.pure(56)
    } yield res
  }

  import cats.syntax.flatMap._ // flatMap
  import cats.syntax.functor._ // map

  // can generalize code
  def mustComputeGeneral[F[_], E](using mc: MonadCancel[F, E]): F[Int] = mc.uncancelable { _ =>
    for {
      _ <- mc.pure("once started, I can't go back...")
      res <- mc.pure(56)
    } yield res
  }

  val mustCompute_vs = mustComputeGeneral[IO, Throwable]

  // allow cancellation listeners
  val mustComputeWithListener = mustCompute.onCancel(IO("I'm being cancelled").void)
  val mustComputeWithListener_v2 = monadCancelIO.onCancel(mustCompute, IO("I'm being cancelled").void) // same
  // .onCancel as extension method
  import cats.effect.syntax.monadCancel._ // .onCancel

  // allow finalizers // guarantee, guaranteeCase
  val aComputationWithFinalizers = monadCancelIO.guaranteeCase(IO(42)) {
    case Succeeded(fa) => fa.flatMap(a => IO(s"succesful: $a").void)
    case Errored(e) => IO(s"failed: $e").void
    case Canceled() => IO(s"Cancelled").void
  }

  // bracket pattern is specific to MonadCancel
  val aComputationWithUsage = monadCancelIO.bracket(IO(42)){ value =>
    IO(s"using the meaning of life: $value")
  } { value =>
    IO(s"releasing the meaning of life...").debug.void
  }


  override def run: IO[Unit] = ???
}
