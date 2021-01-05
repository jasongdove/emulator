import emulator.model.CpuFlags

package object emulator {
  implicit class IntExtensions(value: Int) {
    def toUByte: Int = value & 0xff
    def toSignedByte: Int = value.toByte.toInt
    def wrapAddUByte(add: Int): Int = (value + add).toUByte
    def wrapAddUShort(add: Int): Int = (value + add) % 65536
    def wrapSubUByte(sub: Int): Int = (value - sub).toUByte
  }

  implicit class CpuFlagsValueSetExtensions(flags: CpuFlags.ValueSet) {
    def toByteInstruction: Int = {
      var result = 0x30
      if (flags.contains(CpuFlags.Carry)) result |= 0x01
      if (flags.contains(CpuFlags.Zero)) result |= 0x02
      if (flags.contains(CpuFlags.InterruptDisable)) result |= 0x04
      if (flags.contains(CpuFlags.DecimalMode)) result |= 0x08
      if (flags.contains(CpuFlags.Overflow)) result |= 0x40
      if (flags.contains(CpuFlags.Negative)) result |= 0x80
      result
    }

    def shiftedLeft(oldValue: Int, newValue: Int): CpuFlags.ValueSet =
      flags
        .withCarry((oldValue & 0x80) != 0x00)
        .withZero(newValue == 0x00)
        .withNegative((newValue & 0x80) != 0x00)

    def shiftedRight(oldValue: Int, newValue: Int): CpuFlags.ValueSet =
      flags
        .withCarry((oldValue & 0x01) != 0x00)
        .withZero(newValue == 0x00)
        .withNegative((newValue & 0x80) != 0x00)

    def withCarry(condition: => Boolean): CpuFlags.ValueSet =
      withFlag(condition, CpuFlags.Carry)

    def withZero(condition: => Boolean): CpuFlags.ValueSet =
      withFlag(condition, CpuFlags.Zero)

    def withInterruptDisable(condition: => Boolean): CpuFlags.ValueSet =
      withFlag(condition, CpuFlags.InterruptDisable)

    def withDecimalMode(condition: => Boolean): CpuFlags.ValueSet =
      withFlag(condition, CpuFlags.DecimalMode)

    def withOverflow(condition: => Boolean): CpuFlags.ValueSet =
      withFlag(condition, CpuFlags.Overflow)

    def withNegative(condition: => Boolean): CpuFlags.ValueSet =
      withFlag(condition, CpuFlags.Negative)

    private def withFlag(condition: => Boolean, flag: CpuFlags.Value): CpuFlags.ValueSet =
      if (condition) flags + flag else flags - flag
  }
}
