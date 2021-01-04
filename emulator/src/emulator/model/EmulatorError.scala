package emulator

sealed trait EmulatorError extends Throwable

case class UnsupportedOpCode(opcode: Int) extends EmulatorError {
  override def getMessage: String = s"Unsupported opcode 0x${opcode.toHexString}"
}
