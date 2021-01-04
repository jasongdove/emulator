package semu.model

sealed trait Instruction

object Instruction {
  case object BRK extends Instruction
  case object TAX extends Instruction
  case object LDA extends Instruction
  case object INX extends Instruction
  case object JSR extends Instruction
}
