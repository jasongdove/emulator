package semu

object Main {
  def main(args: Array[String]): Unit = {
    val program = Array(0xa9, 0x05, 0x00).map(_.toUByte)
    val cpu = CPU.load(program)
    cpu.run() match {
      case None =>
        println(cpu)
      case Some(e: UnsupportedOpCode) =>
        println(e.getMessage)
    }
  }
}
