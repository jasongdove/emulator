package object emulator {
  implicit class IntExtensions(value: Int) {
    def toUByte: Int = value % 256
    def wrapAddUByte(add: Int): Int = (value + add) % 256
    def wrapAddUShort(add: Int): Int = (value + add) % 65536
    def wrapSubUByte(sub: Int): Int = (value - sub) % 256
  }
}
