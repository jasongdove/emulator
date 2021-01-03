package semu.model

object CpuStatus extends Enumeration {
  type CpuStatus = Value
  val None, Carry, Zero, InterruptDisable, DecimalMode, BreakCommand, Overflow, Negative = Value
}
