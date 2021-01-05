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
    def withCarry(condition: => Boolean): CpuFlags.ValueSet =
      withFlag(condition, CpuFlags.Carry)

    def withZero(condition: => Boolean): CpuFlags.ValueSet =
      withFlag(condition, CpuFlags.Zero)

    def withOverflow(condition: => Boolean): CpuFlags.ValueSet =
      withFlag(condition, CpuFlags.Overflow)

    def withNegative(condition: => Boolean): CpuFlags.ValueSet =
      withFlag(condition, CpuFlags.Negative)

    private def withFlag(condition: => Boolean, flag: CpuFlags.Value): CpuFlags.ValueSet =
      if (condition) flags + flag else flags - flag
  }
}
