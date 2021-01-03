package object semu {
  implicit class IntExtensions(value: Int) {
    def toUByte: Int = value % 256
    def wrapAddByte(add: Int): Int = (value + add) % 256
  }
}
