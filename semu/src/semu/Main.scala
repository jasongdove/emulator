package semu

import cats.effect._

object Main extends IOApp {
  override def run(args: List[String]): IO[ExitCode] = {
    val program = Array(0xa9, 0x05, 0x00).map(_.toUByte)
    val cpu = CPU.load(program)
    for {
      _ <- cpu.run()
      _ <- IO.delay(println(cpu))
    } yield ExitCode.Success
  }
}
