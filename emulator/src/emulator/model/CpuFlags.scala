package emulator.model

object CpuFlags extends Enumeration {
  type CpuFlags = Value
  val None, Carry, Zero, InterruptDisable, DecimalMode, BreakCommand, Overflow, Negative = Value

  def fromByte(value: Int): ValueSet =
    ValueSet.empty
      .withCarry((value & 0x01) != 0x00)
      .withZero((value & 0x02) != 0x00)
      .withInterruptDisable((value & 0x04) != 0x00)
      .withDecimalMode((value & 0x08) != 0x00)
      .withOverflow((value & 0x40) != 0x00)
      .withNegative((value & 0x80) != 0x00)
}
