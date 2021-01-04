package semu

import semu.model._

case class CPU(memory: Array[Int]) {
  private val STACK: Int = 0x1000

  var registerA: Int = 0
  var registerX: Int = 0
  var registerY: Int = 0
  var stackPointer: Int = 0xff
  var status: CpuStatus.ValueSet = CpuStatus.ValueSet.empty
  var programCounter: Int = memReadUShort(0xfffc)

  def run(): Option[SemuError] = {
    var result: Option[Option[SemuError]] = None

    while (result.isEmpty) {
      val current = memRead(programCounter)
      programCounter += 1
      val counterState = programCounter
      OpCode.opCodes.get(current) match {
        case None =>
          result = Some(Some(UnsupportedOpCode(current)))
        case Some(opcode) =>
          opcode.instruction match {
            case Instruction.BRK => result = Some(None)
            case Instruction.TAX => tax()
            case Instruction.INX => inx()
            case Instruction.LDA => lda(opcode.addressingMode)
            case Instruction.JSR => jsr(opcode)
          }

          if (counterState == programCounter)
            programCounter += opcode.bytes - 1
      }
    }

    result.get
  }

  def memRead(addr: Int): Int =
    memory(addr)

  def memWrite(addr: Int, data: Int): Unit =
    memory(addr) = data

  def memReadUShort(addr: Int): Int = {
    val lo = memRead(addr)
    val hi = memRead(addr + 1)
    (hi << 8) | lo
  }

  def memWriteUShort(addr: Int, data: Int): Unit = {
    val hi = data >> 8
    val lo = data & 0xff
    memWrite(addr, lo)
    memWrite(addr + 1, hi)
  }

  def stackPushUShort(data: Int): Unit = {
    val hi = data >> 8
    val lo = data & 0xff
    stackPush(hi)
    stackPush(lo)
  }

  def stackPush(data: Int): Unit = {
    memWrite(STACK + stackPointer, data)
    stackPointer = stackPointer.wrapSubUByte(1)
  }

  def stackPop(): Int = {
    stackPointer = stackPointer.wrapAddUByte(1)
    memRead(STACK + stackPointer)
  }

  def stackPopUShort(): Int = {
    val lo = stackPop()
    val hi = stackPop()
    (hi << 8) | lo
  }

  override def toString: String =
    s"a: 0x${registerA.toHexString}, x: 0x${registerX.toHexString} s: $status, pc: 0x${programCounter.toHexString}, sp: 0x${stackPointer.toHexString}"

  private def lda(mode: AddressingMode): Unit = {
    val addr = getOperandAddress(mode)
    val value = memRead(addr)
    registerA = value
    updateZeroAndNegativeFlags(registerA)
  }

  private def tax(): Unit = {
    registerX = registerA
    updateZeroAndNegativeFlags(registerX)
  }

  private def inx(): Unit = {
    registerX = registerX.wrapAddUByte(1)
    updateZeroAndNegativeFlags(registerX)
  }

  private def jsr(opcode: OpCode): Unit = {
    val targetAddress = getOperandAddress(opcode.addressingMode)
    stackPushUShort(programCounter + opcode.bytes - 2)
    programCounter = targetAddress
  }

  private def getOperandAddress(mode: AddressingMode): Int =
    mode match {
      case AddressingMode.Immediate =>
        programCounter
      case AddressingMode.ZeroPage =>
        memRead(programCounter)
      case AddressingMode.Absolute =>
        memReadUShort(programCounter)
      case AddressingMode.ZeroPageX =>
        val pos = memRead(programCounter)
        pos.wrapAddUShort(registerX)
      case AddressingMode.ZeroPageY =>
        val pos = memRead(programCounter)
        pos.wrapAddUShort(registerY)
      case AddressingMode.AbsoluteX =>
        val base = memReadUShort(programCounter)
        base.wrapAddUShort(registerX)
      case AddressingMode.AbsoluteY =>
        val base = memReadUShort(programCounter)
        base.wrapAddUShort(registerY)
      case AddressingMode.IndirectX =>
        val base = memRead(programCounter)
        val ptr = base.wrapAddUByte(registerX)
        val lo = memRead(ptr)
        val hi = memRead(ptr + 1)
        (hi << 8) | lo
      case AddressingMode.IndirectY =>
        val base = memRead(programCounter)
        val lo = memRead(base)
        val hi = memRead(base.wrapAddUByte(1))
        val derefBase = (hi << 8) | lo
        derefBase.wrapAddUShort(registerY)
      case AddressingMode.NoneAddressing => ???
    }

  private def updateZeroAndNegativeFlags(result: Int): Unit = {
    status = if (result == 0) status + CpuStatus.Zero else status - CpuStatus.Zero
    status = if ((result & 0x80) != 0) status + CpuStatus.Negative else status - CpuStatus.Negative
  }
}

object CPU {
  def load(program: Array[Int], start: Int = 0x8000): CPU = {
    val memory = Array.ofDim[Int](0xffff)
    Array.copy(program, 0, memory, start, program.length)
    memory(0xfffc) = start
    CPU(memory)
  }
}
