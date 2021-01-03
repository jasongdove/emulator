package semu

sealed trait SemuError extends Throwable

case class UnsupportedOpCode(opcode: Int) extends SemuError {
  override def getMessage: String = s"Unsupported opcode 0x${opcode.toHexString}"
}