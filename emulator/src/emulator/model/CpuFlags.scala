package emulator.model

object CpuFlags extends Enumeration {
  type CpuFlags = Value
  val None, Carry, Zero, InterruptDisable, DecimalMode, BreakCommand, Overflow, Negative = Value
}
