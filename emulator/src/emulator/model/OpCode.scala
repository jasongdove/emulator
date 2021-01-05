package emulator.model

case class OpCode(code: Int, instruction: Instruction, bytes: Int, cycles: Int, addressingMode: Option[AddressingMode])

object OpCode {
  val opCodes: Map[Int, OpCode] = List(
    OpCode(0x00, Instruction.BRK, 1, 7, None),
    OpCode(0xaa, Instruction.TAX, 1, 2, None),
    OpCode(0xe8, Instruction.INX, 1, 2, None),
    OpCode(0xa9, Instruction.LDA, 2, 2, Some(AddressingMode.Immediate)),
    OpCode(0xa5, Instruction.LDA, 2, 3, Some(AddressingMode.ZeroPage)),
    OpCode(0xb5, Instruction.LDA, 2, 4, Some(AddressingMode.ZeroPageX)),
    OpCode(0xad, Instruction.LDA, 3, 4, Some(AddressingMode.Absolute)),
    OpCode(0xbd, Instruction.LDA, 3, 4, Some(AddressingMode.AbsoluteX)),
    OpCode(0xb9, Instruction.LDA, 3, 4, Some(AddressingMode.AbsoluteY)),
    OpCode(0xa1, Instruction.LDA, 2, 6, Some(AddressingMode.IndirectX)),
    OpCode(0xb1, Instruction.LDA, 2, 5, Some(AddressingMode.IndirectY)),
    OpCode(0x20, Instruction.JSR, 3, 6, Some(AddressingMode.Absolute)),
    OpCode(0x60, Instruction.RTS, 1, 6, None),
    OpCode(0x85, Instruction.STA, 2, 3, Some(AddressingMode.ZeroPage)),
    OpCode(0x95, Instruction.STA, 2, 4, Some(AddressingMode.ZeroPageX)),
    OpCode(0x8d, Instruction.STA, 3, 4, Some(AddressingMode.Absolute)),
    OpCode(0x9d, Instruction.STA, 3, 5, Some(AddressingMode.AbsoluteX)),
    OpCode(0x99, Instruction.STA, 3, 5, Some(AddressingMode.AbsoluteY)),
    OpCode(0x81, Instruction.STA, 2, 6, Some(AddressingMode.IndirectX)),
    OpCode(0x91, Instruction.STA, 2, 6, Some(AddressingMode.IndirectY)),
    OpCode(0x29, Instruction.AND, 2, 2, Some(AddressingMode.Immediate)),
    OpCode(0x25, Instruction.AND, 2, 3, Some(AddressingMode.ZeroPage)),
    OpCode(0x35, Instruction.AND, 2, 4, Some(AddressingMode.ZeroPageX)),
    OpCode(0x2d, Instruction.AND, 3, 4, Some(AddressingMode.Absolute)),
    OpCode(0x3d, Instruction.AND, 3, 4, Some(AddressingMode.AbsoluteX)),
    OpCode(0x39, Instruction.AND, 3, 4, Some(AddressingMode.AbsoluteY)),
    OpCode(0x21, Instruction.AND, 2, 6, Some(AddressingMode.IndirectX)),
    OpCode(0x31, Instruction.AND, 2, 5, Some(AddressingMode.IndirectY)),
    OpCode(0x18, Instruction.CLC, 1, 2, None)
  ).map(opcode => opcode.code -> opcode).toMap
}
