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

  def nextY(y: Int, opcode: OpCode): CpuState =
    CpuState(a, x, y, stackPointer, programCounter + opcode.bytes - 1, updateZeroAndNegativeFlags(y))

  def jumped(sp: Int, address: Int): CpuState =
    CpuState(a, x, y, sp, address, flags)

  def clearCarry(): CpuState =
    CpuState(a, x, y, stackPointer, programCounter, flags.withCarry(false))

  def nextFlags(flags: CpuFlags.ValueSet, opcode: OpCode): CpuState =
    CpuState(a, x, y, stackPointer, programCounter + opcode.bytes - 1, flags)

  private def updateZeroAndNegativeFlags(result: Int): CpuFlags.ValueSet =
    flags
      .withZero(result == 0x00)
      .withNegative((result & 0x80) != 0x00)
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
          // println(state)
          // val bytes = memory.memory.slice(state.programCounter, state.programCounter + opcode.bytes).drop(1)
          // println(s"${opcode.instruction} ${bytes.map(b => s"0x${b.toHexString}").mkString(" ")}")
          opcode.instruction match {
            case Instruction.ADC   => loop(adc(opcode, nextState))
            case Instruction.AND   => loop(and(opcode, nextState))
            case Instruction.ASL_A => loop(asl_a(opcode, nextState))
            case Instruction.ASL   => loop(asl(opcode, nextState))
            case Instruction.BCC   => loop(bcc(nextState))
            case Instruction.BCS   => loop(bcs(nextState))
            case Instruction.BEQ   => loop(beq(nextState))
            case Instruction.BIT   => loop(bit(opcode, nextState))
            case Instruction.BMI   => loop(bmi(nextState))
            case Instruction.BNE   => loop(bne(nextState))
            case Instruction.BPL   => loop(bpl(nextState))
            case Instruction.BRK   => CpuRunResult(nextState, None)
            case Instruction.BVC   => loop(bvc(nextState))
            case Instruction.BVS   => loop(bvs(nextState))
            case Instruction.CLC   => loop(clc(opcode, nextState))
            case Instruction.CLD   => loop(cld(opcode, nextState))
            case Instruction.CLI   => loop(cli(opcode, nextState))
            case Instruction.CLV   => loop(clv(opcode, nextState))
            case Instruction.CMP   => loop(cmp(opcode, nextState))
            case Instruction.CPX   => loop(cpx(opcode, nextState))
            case Instruction.CPY   => loop(cpy(opcode, nextState))
            case Instruction.DEC   => loop(dec(opcode, nextState))
            case Instruction.DEX   => loop(dex(opcode, nextState))
            case Instruction.DEY   => loop(dey(opcode, nextState))
            case Instruction.EOR   => loop(eor(opcode, nextState))
            case Instruction.INC   => loop(inc(opcode, nextState))
            case Instruction.INX   => loop(inx(opcode, nextState))
            case Instruction.INY   => loop(iny(opcode, nextState))
            case Instruction.JMP   => loop(jmp(opcode, nextState))
            case Instruction.JSR   => loop(jsr(opcode, nextState))
            case Instruction.LDA   => loop(lda(opcode, nextState))
            case Instruction.LDX   => loop(ldx(opcode, nextState))
            case Instruction.LDY   => loop(ldy(opcode, nextState))
            case Instruction.LSR_A => loop(lsr_a(opcode, nextState))
            case Instruction.LSR   => loop(lsr(opcode, nextState))
            case Instruction.NOP   => loop(nextState)
            case Instruction.ORA   => loop(ora(opcode, nextState))
            case Instruction.PHA   => loop(pha(nextState))
            case Instruction.PHP   => loop(php(nextState))
            case Instruction.PLA   => loop(pla(opcode, nextState))
            case Instruction.PLP   => loop(plp(opcode, nextState))
            case Instruction.ROL_A => loop(rol_a(opcode, nextState))
            case Instruction.ROL   => loop(rol(opcode, nextState))
            case Instruction.ROR_A => loop(ror_a(opcode, nextState))
            case Instruction.ROR   => loop(ror(opcode, nextState))
            case Instruction.RTI   => loop(rti(nextState))
            case Instruction.RTS   => loop(rts(nextState))
            case Instruction.SBC   => loop(sbc(opcode, nextState))
            case Instruction.SEC   => loop(sec(opcode, nextState))
            case Instruction.STA   => loop(sta(opcode, nextState))
            case Instruction.TAX   => loop(tax(opcode, nextState))
            case Instruction.TXA   => loop(txa(opcode, nextState))
          }
      }
    }

    loop(initialState.getOrElse(defaultState))
  }

  private def adc(opcode: OpCode, state: CpuState): CpuState =
    getOperandAddress(opcode, state)
      .map { address =>
        val contents = memory.read(address)
        val carry = if (state.flags.contains(CpuFlags.Carry)) 1 else 0
        val sum = state.a + contents + carry
        val flags = state.flags
          .withCarry(sum > 255)
          .withZero(sum == 0x00)
          .withOverflow(((state.a ^ sum) & (contents ^ sum) & 0x80) != 0x00)
          .withNegative((sum & 0x80) != 0x00)
        CpuState(sum.toUByte, state.x, state.y, state.stackPointer, state.programCounter + opcode.bytes - 1, flags)
      }
      .getOrElse(state.next(opcode))

  private def and(opcode: OpCode, state: CpuState): CpuState =
    getOperandAddress(opcode, state)
      .map { address =>
        val contents = memory.read(address)
        state.nextA(state.a & contents, opcode)
      }
      .getOrElse(state.next(opcode))

  private def asl_a(opcode: OpCode, state: CpuState): CpuState = {
    val result = state.a << 1
    val flags = state.flags.shiftedLeft(state.a, result)
    CpuState(result, state.x, state.y, state.stackPointer, state.programCounter + opcode.bytes - 1, flags)
  }

  private def asl(opcode: OpCode, state: CpuState): CpuState = {
    getOperandAddress(opcode, state)
      .map { address =>
        val contents = memory.read(address)
        val result = contents << 1
        val flags = state.flags.shiftedLeft(contents, result)
        memory.write(address, result)
        state.nextFlags(flags, opcode)
      }
      .getOrElse(state.next(opcode))
  }

  private def bcc(state: CpuState): CpuState =
    branch(!state.flags.contains(CpuFlags.Carry), state)

  private def bcs(state: CpuState): CpuState =
    branch(state.flags.contains(CpuFlags.Carry), state)

  private def beq(state: CpuState): CpuState =
    branch(state.flags.contains(CpuFlags.Zero), state)

  private def bit(opcode: OpCode, state: CpuState): CpuState =
    getOperandAddress(opcode, state)
      .map { address =>
        val contents = memory.read(address)
        val flags = state.flags
          .withZero((contents & state.a) == 0x00)
          .withOverflow((contents & 0x40) != 0x00)
          .withNegative((contents & 0x80) != 0x00)
        state.nextFlags(flags, opcode)
      }
      .getOrElse(state.next(opcode))

  private def bmi(state: CpuState): CpuState =
    branch(state.flags.contains(CpuFlags.Negative), state)

  private def bne(state: CpuState): CpuState =
    branch(!state.flags.contains(CpuFlags.Zero), state)

  private def bpl(state: CpuState): CpuState =
    branch(!state.flags.contains(CpuFlags.Negative), state)

  private def bvc(state: CpuState): CpuState =
    branch(!state.flags.contains(CpuFlags.Overflow), state)

  private def bvs(state: CpuState): CpuState =
    branch(state.flags.contains(CpuFlags.Overflow), state)

  private def clc(opcode: OpCode, state: CpuState): CpuState =
    state.nextFlags(state.flags.withCarry(false), opcode)

  private def cld(opcode: OpCode, state: CpuState): CpuState =
    state.nextFlags(state.flags.withDecimalMode(false), opcode)

  private def cli(opcode: OpCode, state: CpuState): CpuState =
    state.nextFlags(state.flags.withInterruptDisable(false), opcode)

  private def clv(opcode: OpCode, state: CpuState): CpuState =
    state.nextFlags(state.flags.withOverflow(false), opcode)

  private def cmp(opcode: OpCode, state: CpuState): CpuState =
    compare(state.a, opcode, state)

  private def cpx(opcode: OpCode, state: CpuState): CpuState =
    compare(state.x, opcode, state)

  private def cpy(opcode: OpCode, state: CpuState): CpuState =
    compare(state.y, opcode, state)

  private def dec(opcode: OpCode, state: CpuState): CpuState =
    getOperandAddress(opcode, state)
      .map { address =>
        val contents = memory.read(address)
        memory.write(address, contents.wrapSubUByte(1))
        state.next(opcode)
      }
      .getOrElse(state.next(opcode))

  private def dex(opcode: OpCode, state: CpuState): CpuState =
    state.nextX(state.x.wrapSubUByte(1), opcode)

  private def dey(opcode: OpCode, state: CpuState): CpuState =
    state.nextY(state.y.wrapSubUByte(1), opcode)

  private def eor(opcode: OpCode, state: CpuState): CpuState =
    getOperandAddress(opcode, state)
      .map { address =>
        state.nextA(memory.read(address) ^ state.a, opcode)
      }
      .getOrElse(state.next(opcode))

  private def inc(opcode: OpCode, state: CpuState): CpuState =
    getOperandAddress(opcode, state)
      .map { address =>
        val result = memory.read(address).wrapAddUByte(1)
        val flags = state.flags
          .withZero(result == 0x00)
          .withNegative((result & 0x80) != 0x00)
        memory.write(address, result)
        state.nextFlags(flags, opcode)
      }
      .getOrElse(state.next(opcode))

  private def inx(opcode: OpCode, state: CpuState): CpuState =
    state.nextX(state.x.wrapAddUByte(1), opcode)

  private def iny(opcode: OpCode, state: CpuState): CpuState =
    state.nextY(state.y.wrapAddUByte(1), opcode)

  private def jmp(opcode: OpCode, state: CpuState): CpuState =
    getOperandAddress(opcode, state)
      .map { address =>
        state.jumped(state.stackPointer, address)
      }
      .getOrElse(state.next(opcode))

  private def jsr(opcode: OpCode, state: CpuState): CpuState =
    getOperandAddress(opcode, state)
      .map { address =>
        val newState = memory.stackPushUShort(state, state.programCounter + opcode.bytes - 2)
        newState.jumped(newState.stackPointer, address)
      }
      .getOrElse(state.next(opcode))

  private def lda(opcode: OpCode, state: CpuState): CpuState =
    getOperandAddress(opcode, state)
      .map(address => state.nextA(memory.read(address), opcode))
      .getOrElse(state.next(opcode))

  private def ldx(opcode: OpCode, state: CpuState): CpuState =
    getOperandAddress(opcode, state)
      .map(address => state.nextX(memory.read(address), opcode))
      .getOrElse(state.next(opcode))

  private def ldy(opcode: OpCode, state: CpuState): CpuState =
    getOperandAddress(opcode, state)
      .map(address => state.nextY(memory.read(address), opcode))
      .getOrElse(state.next(opcode))

  private def lsr_a(opcode: OpCode, state: CpuState): CpuState = {
    val result = state.a >> 1
    val flags = state.flags
      .withCarry((result & 0x01) != 0x00)
      .withZero(result == 0x00)
      .withNegative(false)
    CpuState(result, state.x, state.y, state.stackPointer, state.programCounter + opcode.bytes - 1, flags)
  }

  private def lsr(opcode: OpCode, state: CpuState): CpuState = {
    getOperandAddress(opcode, state)
      .map { address =>
        val result = memory.read(address) >> 1
        val flags = state.flags
          .withCarry((result & 0x01) != 0x00)
          .withZero(result == 0x00)
          .withNegative(false)
        memory.write(address, result)
        state.nextFlags(flags, opcode)
      }
      .getOrElse(state.next(opcode))
  }

  private def ora(opcode: OpCode, state: CpuState): CpuState =
    getOperandAddress(opcode, state)
      .map { address =>
        state.nextA(memory.read(address) | state.a, opcode)
      }
      .getOrElse(state.next(opcode))

  private def pha(state: CpuState): CpuState =
    memory.stackPush(state, state.a)

  private def php(state: CpuState): CpuState =
    memory.stackPush(state, state.flags.toByteInstruction)

  private def pla(opcode: OpCode, state: CpuState): CpuState = {
    val (nextState, a) = memory.stackPop(state)
    nextState.nextA(a, opcode)
  }

  private def plp(opcode: OpCode, state: CpuState): CpuState = {
    val (nextState, flagsByte) = memory.stackPop(state)
    nextState.nextFlags(CpuFlags.fromByte(flagsByte), opcode)
  }

  private def rol_a(opcode: OpCode, state: CpuState): CpuState = {
    val rotated = state.a << 1
    val result = if (state.flags.contains(CpuFlags.Carry)) rotated | 0x01 else rotated
    val flags = state.flags.shiftedLeft(state.a, result)
    CpuState(result, state.x, state.y, state.stackPointer, state.programCounter + opcode.bytes - 1, flags)
  }

  private def rol(opcode: OpCode, state: CpuState): CpuState = {
    getOperandAddress(opcode, state)
      .map { address =>
        val contents = memory.read(address)
        val rotated = contents << 1
        val result = if (state.flags.contains(CpuFlags.Carry)) rotated | 0x01 else rotated
        val flags = state.flags.shiftedLeft(contents, result)
        memory.write(address, result)
        state.nextFlags(flags, opcode)
      }
      .getOrElse(state.next(opcode))
  }

  private def ror_a(opcode: OpCode, state: CpuState): CpuState = {
    val rotated = state.a >> 1
    val result = if (state.flags.contains(CpuFlags.Carry)) rotated | 0x80 else rotated
    val flags = state.flags.shiftedRight(state.a, result)
    CpuState(result, state.x, state.y, state.stackPointer, state.programCounter + opcode.bytes - 1, flags)
  }

  private def ror(opcode: OpCode, state: CpuState): CpuState = {
    getOperandAddress(opcode, state)
      .map { address =>
        val contents = memory.read(address)
        val rotated = contents >> 1
        val result = if (state.flags.contains(CpuFlags.Carry)) rotated | 0x80 else rotated
        val flags = state.flags.shiftedRight(contents, result)
        memory.write(address, result)
        state.nextFlags(flags, opcode)
      }
      .getOrElse(state.next(opcode))
  }

  private def rti(state: CpuState): CpuState = {
    val (state1, flagsByte) = memory.stackPopUShort(state)
    val (state2, programCounter) = memory.stackPopUShort(state1)
    CpuState(state2.a, state2.x, state2.y, state2.stackPointer, programCounter, CpuFlags.fromByte(flagsByte))
  }

  private def rts(state: CpuState): CpuState = {
    val (newState, programCounter) = memory.stackPopUShort(state)
    newState.jumped(newState.stackPointer, programCounter + 1)
  }

  private def sbc(opcode: OpCode, state: CpuState): CpuState =
    getOperandAddress(opcode, state)
      .map { address =>
        val contents = memory.read(address)
        val carry = if (!state.flags.contains(CpuFlags.Carry)) 1 else 0
        val sum = state.a - contents - carry
        val flags = state.flags
          .withCarry(sum < 0x00)
          .withZero(sum == 0x00)
          .withOverflow(((state.a ^ sum) & (contents ^ sum) & 0x80) != 0x00)
          .withNegative((sum & 0x80) != 0x00)
        CpuState(sum.toUByte, state.x, state.y, state.stackPointer, state.programCounter + opcode.bytes - 1, flags)
      }
      .getOrElse(state.next(opcode))

  private def sec(opcode: OpCode, state: CpuState): CpuState =
    state.nextFlags(state.flags.withCarry(true), opcode)

  private def sta(opcode: OpCode, state: CpuState): CpuState =
    getOperandAddress(opcode, state)
      .map { address =>
        memory.write(address, state.a)
        state.next(opcode)
      }
      .getOrElse(state.next(opcode))

  private def tax(opcode: OpCode, state: CpuState): CpuState =
    state.nextX(state.a, opcode)

  private def txa(opcode: OpCode, state: CpuState): CpuState =
    state.nextA(state.x, opcode)

  private def branch(condition: => Boolean, state: CpuState): CpuState =
    if (condition) {
      val offset = memory.read(state.programCounter).toSignedByte
      state.jumped(state.stackPointer, state.programCounter + 1 + offset)
    } else state.next()

  private def compare(input: Int, opcode: OpCode, state: CpuState): CpuState =
    getOperandAddress(opcode, state)
      .map { address =>
        val contents = memory.read(address)
        val diff = input - contents
        val flags = state.flags
          .withCarry(input >= contents)
          .withZero(diff == 0x00)
          .withNegative((diff & 0x80) != 0x00)
        state.nextFlags(flags, opcode)
      }
      .getOrElse(state.next(opcode))

  private def getOperandAddress(opcode: OpCode, state: CpuState): Option[Int] =
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
          case AddressingMode.Indirect =>
            val pos = memory.readUShort(state.programCounter)
            memory.readUShort(pos)
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
