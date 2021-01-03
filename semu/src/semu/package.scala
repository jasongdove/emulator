package object semu {
  implicit class IntExtensions(value: Int) {
    def toUByte: Int = value % 0xff
  }
}
