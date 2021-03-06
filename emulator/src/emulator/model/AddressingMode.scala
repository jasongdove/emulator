package emulator.model

sealed trait AddressingMode

object AddressingMode {
  // TODO: Accumulator instead of _A versions of instructions?
  case object Immediate extends AddressingMode
  case object ZeroPage extends AddressingMode
  case object ZeroPageX extends AddressingMode
  case object ZeroPageY extends AddressingMode
  case object Absolute extends AddressingMode
  case object AbsoluteX extends AddressingMode
  case object AbsoluteY extends AddressingMode
  case object Indirect extends AddressingMode
  case object IndirectX extends AddressingMode
  case object IndirectY extends AddressingMode
}
