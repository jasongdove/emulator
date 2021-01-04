package emulator

import cats.effect.IO
import weaver._

object AddressingModeSuite extends SimpleIOSuite {
  simpleTest("LDA Immediate") {
    val program = Array(0xa9, 0x05, 0x00)
    val cpu = CPU.load(program)
    for {
      _ <- IO(cpu.run())
      _ <- expect(cpu.registerA == 5).failFast
    } yield success
  }

  simpleTest("LDA ZeroPage") {
    val program = Array(0xa5, 0xf0, 0x00)
    val cpu = CPU.load(program)
    cpu.memWrite(0xf0, 0x05)
    for {
      _ <- IO(cpu.run())
      _ <- expect(cpu.registerA == 5).failFast
    } yield success
  }

  simpleTest("LDA ZeroPage X") {
    val program = Array(0xb5, 0xf0, 0x00)
    val cpu = CPU.load(program)
    cpu.registerX = 0x01
    cpu.memWrite(0xf1, 0x05)
    for {
      _ <- IO(cpu.run())
      _ <- expect(cpu.registerA == 5).failFast
    } yield success
  }

  simpleTest("LDA Absolute") {
    val program = Array(0xad, 0x00, 0xf0, 0x00)
    val cpu = CPU.load(program)
    cpu.memWriteUShort(0xf000, 0x05)
    for {
      _ <- IO(cpu.run())
      _ <- expect(cpu.registerA == 0x05).failFast
    } yield success
  }

  simpleTest("LDA Absolute X") {
    val program = Array(0xbd, 0x00, 0xf0, 0x00)
    val cpu = CPU.load(program)
    cpu.registerX = 0x01
    cpu.memWriteUShort(0xf001, 0x05)
    for {
      _ <- IO(cpu.run())
      _ <- expect(cpu.registerA == 0x05).failFast
    } yield success
  }

  simpleTest("LDA Absolute Y") {
    val program = Array(0xb9, 0x00, 0xf0, 0x00)
    val cpu = CPU.load(program)
    cpu.registerY = 0x01
    cpu.memWriteUShort(0xf001, 0x05)
    for {
      _ <- IO(cpu.run())
      _ <- expect(cpu.registerA == 0x05).failFast
    } yield success
  }

  simpleTest("LDA Indirect X") {
    val program = Array(0xa1, 0xf0, 0x00)
    val cpu = CPU.load(program)
    cpu.registerX = 0x01
    cpu.memWriteUShort(0xf1, 0xf001)
    cpu.memWriteUShort(0xf001, 0x05)
    for {
      _ <- IO(cpu.run())
      _ <- expect(cpu.registerA == 5).failFast
    } yield success
  }

  simpleTest("LDA Indirect Y") {
    val program = Array(0xb1, 0xf0, 0x00)
    val cpu = CPU.load(program)
    cpu.registerY = 0x01
    cpu.memWriteUShort(0xf0, 0xf001)
    cpu.memWriteUShort(0xf002, 0x05)
    for {
      _ <- IO(cpu.run())
      _ <- expect(cpu.registerA == 5).failFast
    } yield success
  }
}
