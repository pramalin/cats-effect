package com.rockthejvm.part2effects

object Effects {


  case class MyIO[A](unsafeRun: () => A) {
    def map[B](f: A => B): MyIO[B] =
      MyIO(() => f(unsafeRun()))

    def flatMap[B](f: A => MyIO[B]): MyIO[B] =
      MyIO(() => f(unsafeRun()).unsafeRun())
  }

  val anIO: MyIO[Int] = MyIO(() => {
    println("I'm writting something...")
    42
  })
  def main(args: Array[String]): Unit = {
   println(s"io: ${anIO.unsafeRun()}")
  }
}
