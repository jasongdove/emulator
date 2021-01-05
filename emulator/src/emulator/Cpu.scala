package emulator

import emulator.model._

import scala.annotation.tailrec

case class CpuState(a: Int, x: Int, y: Int, stackPointer: Int, programCounter: Int, flags: CpuFlags.ValueSet) {
  override def toString: String =
    s"a: 0x${a.toHexString}, x: 0x${x.toHexString} s: $flags, pc: 0x${programCounter.toHexString}, sp: 0x${stackPointer.toHexString}"

  def next(): CpuState =
    CpuState(a, x, y, stackPointer, programCounter + 1, flags)

  def next(opcode: OpCode): CpuState =
    CpuState(a, x, y, stackPointer, programCounter + opcode.bytes - 1, flags)

  def nextA(a: Int, opcode: OpCode): CpuState =
    CpuState(a, x, y, stackPointer, programCounter + opcode.bytes - 1, updateZeroAndNegativeFlags(a))

  def nextX(x: Int, opcode: OpCode): CpuState =
    CpuState(a, x, y, stackPointer, programCounter + opcode.bytes - 1, updateZeroAndNegativeFlags(x))

  def jumped(sp: Int, add: Int): CpuState =
    CpuState(a, x, y, sp, add, flags)

  def clearCarry(): CpuState =
    CpuState(a, x, y, stackPointer, programCounter, flags.withCarry(false))

  private def updateZeroAndNegativeFlags(result: Int): CpuFlags.ValueSet =
    flags
      .withZero(result == 0)
      .withNegative((result & 0x80) != 0)
}

case class CpuRunResult(state: CpuState, error: Option[EmulatorError])

case class Cpu(memory: MemoryMap) {
  private lazy val defaultState: CpuState = {
    val initialStackPointer = 0xff
    val initialProgramCounter = memory.readUShort(0xfffc)
    CpuState(0, 0, 0, initialStackPointer, initialProgramCounter, CpuFlags.ValueSet.empty)
  }

  def run(initialState: Option[CpuState] = None): CpuRunResult = {
    @tailrec
    def loop(state: CpuState): CpuRunResult = {
      val current = memory.read(state.programCounter)
      val nextState = state.next()

      OpCode.opCodes.get(current) match {
        case None         => CpuRunResult(nextState, Some(UnsupportedOpCode(current)))
        case Some(opcode) =>
//          println(state)
//          val bytes = memory.memory.slice(state.programCounter, state.programCounter + opcode.bytes).drop(1)
//          println(s"${opcode.instruction} ${bytes.map(b => s"0x${b.toHexString}").mkString(" ")}")
          opcode.instruction match {
            case Instruction.BRK => CpuRunResult(nextState, None)
            case Instruction.TAX => loop(tax(opcode, nextState))
            case Instruction.INX => loop(inx(opcode, nextState))
            case Instruction.LDA => loop(lda(opcode, nextState))
            case Instruction.JSR => loop(jsr(opcode, nextState))
            case Instruction.RTS => loop(rts(nextState))
            case Instruction.STA => loop(sta(opcode, nextState))
            case Instruction.AND => loop(and(opcode, nextState))
            case Instruction.CLC => loop(clc(nextState))
            case Instruction.ADC => loop(adc(opcode, nextState))
          }
      }
    }

    loop(initialState.getOrElse(defaultState))
  }

  def lda(opcode: OpCode, state: CpuState): CpuState = {
    val address = getOperandAddress(opcode, state)
    address.map(addr => state.nextA(memory.read(addr), opcode)).getOrElse(state)
  }

  def tax(opcode: OpCode, state: CpuState): CpuState =
    state.nextX(state.a, opcode)

  def inx(opcode: OpCode, state: CpuState): CpuState =
    state.nextX(state.x.wrapAddUByte(1), opcode)

  def jsr(opcode: OpCode, state: CpuState): CpuState =
    getOperandAddress(opcode, state)
      .map { address =>
        val newState = memory.stackPushUShort(state, state.programCounter + opcode.bytes - 2)
        newState.jumped(newState.stackPointer, address)
      }
      .getOrElse(state)

  def rts(state: CpuState): CpuState = {
    val (newState, programCounter) = memory.stackPopUShort(state)
    newState.jumped(newState.stackPointer, programCounter + 1)
  }

  def sta(opcode: OpCode, state: CpuState): CpuState =
    getOperandAddress(opcode, state)
      .map { address =>
        memory.write(address, state.a)
        state.next(opcode)
      }
      .getOrElse(state)

  def and(opcode: OpCode, state: CpuState): CpuState =
    getOperandAddress(opcode, state)
      .map { address =>
        val contents = memory.read(address)
        state.nextA(state.a & contents, opcode)
      }
      .getOrElse(state)

  def clc(state: CpuState): CpuState = state.clearCarry()

  def adc(opcode: OpCode, state: CpuState): CpuState =
    getOperandAddress(opcode, state)
      .map { address =>
        val contents = memory.read(address)
        val carry = if (state.flags.contains(CpuFlags.Carry)) 1 else 0
        val sum = state.a + contents + carry
        val flags = state.flags
          .withCarry(sum > 255)
          .withZero(sum == 0)
          .withOverflow(((state.a ^ sum) & (contents ^ sum) & 0x80) != 0)
          .withNegative((sum & 0x80) != 0)
        CpuState(sum.toUByte, state.x, state.y, state.stackPointer, state.programCounter + opcode.bytes - 1, flags)
      }
      .getOrElse(state)

  def getOperandAddress(opcode: OpCode, state: CpuState): Option[Int] =
    opcode.addressingMode match {
      case None => None
      case Some(mode) =>
        val address = mode match {
          case AddressingMode.Immediate =>
            state.programCounter
          case AddressingMode.ZeroPage =>
            memory.read(state.programCounter)
          case AddressingMode.Absolute =>
            memory.readUShort(state.programCounter)
          case AddressingMode.ZeroPageX =>
            val pos = memory.read(state.programCounter)
            pos.wrapAddUShort(state.x)
          case AddressingMode.ZeroPageY =>
            val pos = memory.read(state.programCounter)
            pos.wrapAddUShort(state.y)
          case AddressingMode.AbsoluteX =>
            val base = memory.readUShort(state.programCounter)
            base.wrapAddUShort(state.x)
          case AddressingMode.AbsoluteY =>
            val base = memory.readUShort(state.programCounter)
            base.wrapAddUShort(state.y)
          case AddressingMode.IndirectX =>
            val base = memory.read(state.programCounter)
            val ptr = base.wrapAddUByte(state.x)
            val lo = memory.read(ptr)
            val hi = memory.read(ptr + 1)
            (hi << 8) | lo
          case AddressingMode.IndirectY =>
            val base = memory.read(state.programCounter)
            val lo = memory.read(base)
            val hi = memory.read(base.wrapAddUByte(1))
            val derefBase = (hi << 8) | lo
            derefBase.wrapAddUShort(state.y)
        }
        Some(address)
    }
}

object Cpu {
  def load(program: Array[Int], start: Int = 0x8000): Cpu = {
    val memory = Array.ofDim[Int](0xffff)
    Array.copy(program, 0, memory, start, program.length)
    memory(0xfffc) = start
    Cpu(MemoryMap(memory))
  }
}
