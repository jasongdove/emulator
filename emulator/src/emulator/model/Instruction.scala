package emulator.model

sealed trait Instruction

object Instruction {
  case object BRK extends Instruction
  case object TAX extends Instruction
  case object TXA extends Instruction
  case object LDA extends Instruction
  case object LDX extends Instruction
  case object INX extends Instruction
  case object DEX extends Instruction
  case object JSR extends Instruction
  case object RTS extends Instruction
  case object STA extends Instruction
  case object AND extends Instruction
  case object CLC extends Instruction
  case object SEC extends Instruction
  case object ADC extends Instruction
  case object SBC extends Instruction
  case object CMP extends Instruction
  case object BEQ extends Instruction
  case object BNE extends Instruction
  case object BPL extends Instruction
  case object BCS extends Instruction
  case object CPX extends Instruction
  case object LSR_A extends Instruction
  case object LSR extends Instruction
}
