package emulator

case class MemoryMap(memory: Array[Int]) {
  private val STACK: Int = 0x1000

  def read(addr: Int): Int =
    memory(addr)

  def write(addr: Int, data: Int): Unit =
    memory(addr) = data

  def readUShort(addr: Int): Int = {
    val lo = read(addr)
    val hi = read(addr + 1)
    (hi << 8) | lo
  }

  def writeUShort(addr: Int, data: Int): Unit = {
    val hi = data >> 8
    val lo = data & 0xff
    write(addr, lo)
    write(addr + 1, hi)
  }

  def stackPush(state: CpuState, data: Int): CpuState = {
    write(STACK + state.stackPointer, data)
    CpuState(state.a, state.x, state.y, state.stackPointer.wrapSubUByte(1), state.programCounter, state.flags)
  }

  def stackPushUShort(state: CpuState, data: Int): CpuState = {
    val hi = data >> 8
    val lo = data & 0xff
    val hiState = stackPush(state, hi)
    stackPush(hiState, lo)
  }

  def stackPop(state: CpuState): (CpuState, Int) = {
    val stackPointer = state.stackPointer.wrapAddUByte(1)
    val nextState = CpuState(state.a, state.x, state.y, stackPointer, state.programCounter, state.flags)
    nextState -> read(STACK + stackPointer)
  }

  def stackPopUShort(state: CpuState): (CpuState, Int) = {
    val (loState, lo) = stackPop(state)
    val (hiState, hi) = stackPop(loState)
    hiState -> ((hi << 8) | lo)
  }
}
