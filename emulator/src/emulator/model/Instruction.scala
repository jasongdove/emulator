package emulator.model

sealed trait Instruction

object Instruction {
  case object ADC extends Instruction
  case object AND extends Instruction
  case object ASL_A extends Instruction
  case object ASL extends Instruction
  case object BCC extends Instruction
  case object BCS extends Instruction
  case object BEQ extends Instruction
  case object BIT extends Instruction
  case object BMI extends Instruction
  case object BNE extends Instruction
  case object BPL extends Instruction
  case object BRK extends Instruction
  case object BVC extends Instruction
  case object BVS extends Instruction
  case object CLC extends Instruction
  case object CLD extends Instruction
  case object CLI extends Instruction
  case object CLV extends Instruction
  case object CMP extends Instruction
  case object CPX extends Instruction
  case object CPY extends Instruction
  case object DEC extends Instruction
  case object DEX extends Instruction
  case object DEY extends Instruction
  case object INX extends Instruction
  case object JMP extends Instruction
  case object JSR extends Instruction
  case object LDA extends Instruction
  case object LDX extends Instruction
  case object LDY extends Instruction
  case object LSR_A extends Instruction
  case object LSR extends Instruction
  case object NOP extends Instruction
  case object RTS extends Instruction
  case object SBC extends Instruction
  case object SEC extends Instruction
  case object STA extends Instruction
  case object TAX extends Instruction
  case object TXA extends Instruction
}
