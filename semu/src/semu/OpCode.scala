package semu

import semu.model.AddressingMode

case class OpCode(code: Int, instruction: Instruction, bytes: Int, cycles: Int, addressingMode: AddressingMode)

object OpCode {
  val opCodes: Map[Int, OpCode] = List(
    OpCode(0x00, Instruction.BRK, 1, 7, AddressingMode.NoneAddressing),
    OpCode(0xaa, Instruction.TAX, 1, 2, AddressingMode.NoneAddressing),
    OpCode(0xe8, Instruction.INX, 1, 2, AddressingMode.NoneAddressing),
    OpCode(0xa9, Instruction.LDA, 2, 2, AddressingMode.Immediate),
    OpCode(0xa5, Instruction.LDA, 2, 3, AddressingMode.ZeroPage),
    OpCode(0xb5, Instruction.LDA, 2, 4, AddressingMode.ZeroPageX),
    OpCode(0xad, Instruction.LDA, 3, 4, AddressingMode.Absolute),
    OpCode(0xbd, Instruction.LDA, 3, 4, AddressingMode.AbsoluteX),
    OpCode(0xb9, Instruction.LDA, 3, 4, AddressingMode.AbsoluteY),
    OpCode(0xa1, Instruction.LDA, 2, 6, AddressingMode.IndirectX),
    OpCode(0xb1, Instruction.LDA, 2, 5, AddressingMode.IndirectY)
  ).map(opcode => opcode.code -> opcode).toMap
}
