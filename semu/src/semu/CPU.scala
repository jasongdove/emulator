package semu

import cats.effect.IO
import semu.model.CpuStatus

import scala.annotation.tailrec

case class CPU(memory: Array[Int], start: Int) {
  var registerA: Int = 0
  var registerX: Int = 0
  var status: CpuStatus.ValueSet = CpuStatus.ValueSet.empty
  var programCounter: Int = start

  def run(): IO[Unit] = {
    @tailrec
    def loop(): IO[Unit] = {
      val current = memory(programCounter)
      programCounter += 1
      current match {
        case 0xa9 =>
          val param = memory(programCounter)
          programCounter += 1
          lda(param)
          loop()
        case 0xaa =>
          tax()
          loop()
        case 0xe8 =>
          inx()
          loop()
        case 0x00 =>
          IO.unit
        case opcode =>
          IO.raiseError(UnsupportedOpCode(opcode))
      }
    }

    loop()
  }

  private def lda(value: Int): Unit = {
    registerA = value
    updateZeroAndNegativeFlags(registerA)
  }

  private def tax(): Unit = {
    registerX = registerA
    updateZeroAndNegativeFlags(registerX)
  }

  private def inx(): Unit = {
    registerX = registerX.wrapAddByte(1)
    updateZeroAndNegativeFlags(registerX)
  }

  private def updateZeroAndNegativeFlags(result: Int): Unit = {
    status = if (result == 0) status + CpuStatus.Zero else status - CpuStatus.Zero
    status = if ((result & 0x80) != 0) status + CpuStatus.Negative else status - CpuStatus.Negative
  }

  override def toString: String =
    s"a: $registerA, x: $registerX s: $status, pc: 0x${programCounter.toHexString}"
}

object CPU {
  def load(program: Array[Int]): CPU = {
    val start = 0x8000
    val memory = Array.ofDim[Int](0xffff)
    Array.copy(program, 0, memory, start, program.length)
    CPU(memory, start)
  }
}
