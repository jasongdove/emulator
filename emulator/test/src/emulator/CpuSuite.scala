package emulator

import cats.effect.IO
import emulator.model.CpuStatus
import weaver._

object CpuSuite extends SimpleIOSuite {
  simpleTest("memWriteUShort should write in little-endian") {
    val program = Array(0xa9, 0x00, 0x00)
    val cpu = CPU.load(program)
    for {
      _ <- IO(cpu.memWriteUShort(0xf000, 0x8090))
      _ <- expect(cpu.memory(0xf000) == 0x90).failFast
      _ <- expect(cpu.memory(0xf001) == 0x80).failFast
    } yield success
  }

  simpleTest("memReadUShort should read in little-endian") {
    val program = Array(0xa9, 0x00, 0x00)
    val cpu = CPU.load(program)
    cpu.memWrite(0xf000, 0x90)
    cpu.memWrite(0xf001, 0x80)
    for {
      result <- IO(cpu.memReadUShort(0xf000))
      _ <- expect(result == 0x8090).failFast
    } yield success
  }

  simpleTest("LDA should set zero flag") {
    val program = Array(0xa9, 0x00, 0x00)
    val cpu = CPU.load(program)
    for {
      _ <- IO(cpu.run())
      _ <- expect(cpu.registerA == 0).failFast
      _ <- expect(cpu.status.contains(CpuStatus.Zero)).failFast
    } yield success
  }

  simpleTest("LDA should set negative flag") {
    val program = Array(0xa9, 0xff, 0x00)
    val cpu = CPU.load(program)
    for {
      _ <- IO(cpu.run())
      _ <- expect(cpu.registerA == 0xff).failFast
      _ <- expect(cpu.status.contains(CpuStatus.Negative)).failFast
    } yield success
  }

  simpleTest("TAX should set zero flag") {
    val program = Array(0xaa, 0x00)
    val cpu = CPU.load(program)
    cpu.registerA = 0
    for {
      _ <- IO(cpu.run())
      _ <- expect(cpu.registerX == 0).failFast
      _ <- expect(cpu.status.contains(CpuStatus.Zero)).failFast
    } yield success
  }

  simpleTest("TAX should set negative flag") {
    val program = Array(0xaa, 0x00)
    val cpu = CPU.load(program)
    cpu.registerA = 0xff
    for {
      _ <- IO(cpu.run())
      _ <- expect(cpu.registerX == 0xff).failFast
      _ <- expect(cpu.status.contains(CpuStatus.Negative)).failFast
    } yield success
  }

  simpleTest("INX should set zero flag") {
    val program = Array(0xe8, 0x00)
    val cpu = CPU.load(program)
    cpu.registerX = 0xff
    for {
      _ <- IO(cpu.run())
      _ <- expect(cpu.registerX == 0).failFast
      _ <- expect(cpu.status.contains(CpuStatus.Zero)).failFast
    } yield success
  }

  simpleTest("INX should set negative flag") {
    val program = Array(0xe8, 0x00)
    val cpu = CPU.load(program)
    cpu.registerX = 0x7f
    for {
      _ <- IO(cpu.run())
      _ <- expect(cpu.registerX == 0x80).failFast
      _ <- expect(cpu.status.contains(CpuStatus.Negative)).failFast
    } yield success
  }

  simpleTest("INX should overflow correctly") {
    val program = Array(0xe8, 0xe8, 0x00)
    val cpu = CPU.load(program)
    cpu.registerX = 0xff
    for {
      _ <- IO(cpu.run())
      _ <- expect(cpu.registerX == 1).failFast
    } yield success
  }

  simpleTest("JSR should set stack and program counter correctly") {
    val program = Array(0x20, 0x0d, 0x06)
    val cpu = CPU.load(program, 0x500)
    for {
      _ <- IO(cpu.run())
      // stack should contain return address - 1 (so address of last byte in JSR)
      _ <- expect(cpu.stackPopUShort() == 0x0502).failFast
      // we end up with address + 1 after we terminate because we read the 0x00 (BRK) at address
      _ <- expect(cpu.programCounter == 0x060e).failFast
    } yield success
  }
}
