package semu

import cats.effect.IO
import semu.model.CpuStatus

import scala.annotation.tailrec

case class CPU(memory: Array[Int], start: Int) {
  private var _registerA: Int = 0
  private var _status = CpuStatus.ValueSet.empty
  private var _programCounter: Int = start

  def run(): IO[Unit] = {
    @tailrec
    def loop(): IO[Unit] = {
      memory(_programCounter) match {
        case 0xa9 =>
          val param = memory(_programCounter + 1)
          _registerA = param
          _programCounter += 2
          _status = if (registerA == 0) _status + CpuStatus.Zero else _status - CpuStatus.Zero
          _status = if ((registerA & 0x80) != 0) _status + CpuStatus.Negative else _status - CpuStatus.Negative
          loop()
        case 0x00 =>
          IO.unit
        case opcode =>
          IO.raiseError(UnsupportedOpCode(opcode))
      }
    }

    loop()
  }

  def registerA: Int = _registerA
  def status: CpuStatus.ValueSet = _status
  def programCounter: Int = _programCounter

  override def toString: String =
    s"a: ${_registerA}, s: ${_status}, pc: 0x${_programCounter.toHexString}"
}

object CPU {
  def load(program: Array[Int]): CPU = {
    val start = 0x8000
    val memory = Array.ofDim[Int](0xffff)
    Array.copy(program, 0, memory, start, program.length)
    CPU(memory, start)
  }
}
