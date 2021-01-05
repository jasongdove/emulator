package emulator.model

sealed trait Instruction

object Instruction {
  case object BRK extends Instruction
  case object TAX extends Instruction
  case object LDA extends Instruction
  case object INX extends Instruction
  case object JSR extends Instruction
  case object RTS extends Instruction
  case object STA extends Instruction
  case object AND extends Instruction
  case object CLC extends Instruction
}
