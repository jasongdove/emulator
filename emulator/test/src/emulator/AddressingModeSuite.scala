package emulator

import cats.effect.IO
import emulator.model.CpuFlags
import weaver._

object AddressingModeSuite extends SimpleIOSuite {
  simpleTest("LDA Immediate") {
    val program = Array(0xa9, 0x05, 0x00)
    val cpu = Cpu.load(program)
    for {
      result <- IO(cpu.run())
      _ <- expect(result.state.a == 0x05).failFast
    } yield success
  }

  simpleTest("LDA ZeroPage") {
    val program = Array(0xa5, 0xf0, 0x00)
    val cpu = Cpu.load(program)
    cpu.memory.write(0xf0, 0x05)
    for {
      result <- IO(cpu.run())
      _ <- expect(result.state.a == 0x05).failFast
    } yield success
  }

  simpleTest("LDA ZeroPage X") {
    val program = Array(0xb5, 0xf0, 0x00)
    val cpu = Cpu.load(program)
    cpu.memory.write(0xf1, 0x05)
    for {
      result <- IO(cpu.run(Some(CpuState(0, x = 0x01, 0, 0xff, 0x8000, CpuFlags.ValueSet.empty))))
      _ <- expect(result.state.a == 0x05).failFast
    } yield success
  }

  simpleTest("LDA Absolute") {
    val program = Array(0xad, 0x00, 0xf0, 0x00)
    val cpu = Cpu.load(program)
    cpu.memory.writeUShort(0xf000, 0x05)
    for {
      result <- IO(cpu.run())
      _ <- expect(result.state.a == 0x05).failFast
    } yield success
  }

  simpleTest("LDA Absolute X") {
    val program = Array(0xbd, 0x00, 0xf0, 0x00)
    val cpu = Cpu.load(program)
    cpu.memory.writeUShort(0xf001, 0x05)
    for {
      result <- IO(cpu.run(Some(CpuState(0, x = 0x01, 0, 0xff, 0x8000, CpuFlags.ValueSet.empty))))
      _ <- expect(result.state.a == 0x05).failFast
    } yield success
  }

  simpleTest("LDA Absolute Y") {
    val program = Array(0xb9, 0x00, 0xf0, 0x00)
    val cpu = Cpu.load(program)
    cpu.memory.writeUShort(0xf001, 0x05)
    for {
      result <- IO(cpu.run(Some(CpuState(0, 0, y = 0x01, 0xff, 0x8000, CpuFlags.ValueSet.empty))))
      _ <- expect(result.state.a == 0x05).failFast
    } yield success
  }

  simpleTest("LDA Indirect X") {
    val program = Array(0xa1, 0xf0, 0x00)
    val cpu = Cpu.load(program)
    cpu.memory.writeUShort(0xf1, 0xf001)
    cpu.memory.writeUShort(0xf001, 0x05)
    for {
      result <- IO(cpu.run(Some(CpuState(0, x = 0x01, 0, 0xff, 0x8000, CpuFlags.ValueSet.empty))))
      _ <- expect(result.state.a == 0x05).failFast
    } yield success
  }

  simpleTest("LDA Indirect Y") {
    val program = Array(0xb1, 0xf0, 0x00)
    val cpu = Cpu.load(program)
    cpu.memory.writeUShort(0xf0, 0xf001)
    cpu.memory.writeUShort(0xf002, 0x05)
    for {
      result <- IO(cpu.run(Some(CpuState(0, 0, y = 0x01, 0xff, 0x8000, CpuFlags.ValueSet.empty))))
      _ <- expect(result.state.a == 0x05).failFast
    } yield success
  }
}
