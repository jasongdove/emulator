package emulator

import cats.effect.IO
import emulator.model.CpuFlags
import weaver._

object CpuSuite extends SimpleIOSuite {
  simpleTest("memWriteUShort should write in little-endian") {
    val program = Array(0xa9, 0x00, 0x00)
    val cpu = Cpu.load(program)
    for {
      _ <- IO(cpu.memory.writeUShort(0xf000, 0x8090))
      _ <- expect(cpu.memory.read(0xf000) == 0x90).failFast
      _ <- expect(cpu.memory.read(0xf001) == 0x80).failFast
    } yield success
  }

  simpleTest("memReadUShort should read in little-endian") {
    val program = Array(0xa9, 0x00, 0x00)
    val cpu = Cpu.load(program)
    cpu.memory.write(0xf000, 0x90)
    cpu.memory.write(0xf001, 0x80)
    for {
      result <- IO(cpu.memory.readUShort(0xf000))
      _ <- expect(result == 0x8090).failFast
    } yield success
  }

  simpleTest("LDA should set zero flag") {
    val program = Array(0xa9, 0x00, 0x00)
    val cpu = Cpu.load(program)
    for {
      result <- IO(cpu.run())
      _ <- expect(result.state.a == 0x00).failFast
      _ <- expect(result.state.flags.contains(CpuFlags.Zero)).failFast
    } yield success
  }

  simpleTest("LDA should set negative flag") {
    val program = Array(0xa9, 0xff, 0x00)
    val cpu = Cpu.load(program)
    for {
      result <- IO(cpu.run())
      _ <- expect(result.state.a == 0xff).failFast
      _ <- expect(result.state.flags.contains(CpuFlags.Negative)).failFast
    } yield success
  }

  simpleTest("TAX should set zero flag") {
    val program = Array(0xaa, 0x00)
    val cpu = Cpu.load(program)
    for {
      result <- IO(cpu.run(Some(CpuState(a = 0, 0, 0, 0xff, 0x8000, CpuFlags.ValueSet.empty))))
      _ <- expect(result.state.x == 0x00).failFast
      _ <- expect(result.state.flags.contains(CpuFlags.Zero)).failFast
    } yield success
  }

  simpleTest("TAX should set negative flag") {
    val program = Array(0xaa, 0x00)
    val cpu = Cpu.load(program)
    for {
      result <- IO(cpu.run(Some(CpuState(a = 0xff, 0, 0, 0xff, 0x8000, CpuFlags.ValueSet.empty))))
      _ <- expect(result.state.x == 0xff).failFast
      _ <- expect(result.state.flags.contains(CpuFlags.Negative)).failFast
    } yield success
  }

  simpleTest("INX should set zero flag") {
    val program = Array(0xe8, 0x00)
    val cpu = Cpu.load(program)
    for {
      result <- IO(cpu.run(Some(CpuState(0, x = 0xff, 0, 0xff, 0x8000, CpuFlags.ValueSet.empty))))
      _ <- expect(result.state.x == 0x00).failFast
      _ <- expect(result.state.flags.contains(CpuFlags.Zero)).failFast
    } yield success
  }

  simpleTest("INX should set negative flag") {
    val program = Array(0xe8, 0x00)
    val cpu = Cpu.load(program)
    for {
      result <- IO(cpu.run(Some(CpuState(0, x = 0x7f, 0, 0xff, 0x8000, CpuFlags.ValueSet.empty))))
      _ <- expect(result.state.x == 0x80).failFast
      _ <- expect(result.state.flags.contains(CpuFlags.Negative)).failFast
    } yield success
  }

  simpleTest("INX should overflow correctly") {
    val program = Array(0xe8, 0xe8, 0x00)
    val cpu = Cpu.load(program)
    for {
      result <- IO(cpu.run(Some(CpuState(0, x = 0xff, 0, 0xff, 0x8000, CpuFlags.ValueSet.empty))))
      _ <- expect(result.state.x == 0x01).failFast
    } yield success
  }

  simpleTest("JSR should set stack and program counter correctly") {
    val program = Array(0x20, 0x0d, 0x06)
    val cpu = Cpu.load(program, 0x500)
    for {
      result <- IO(cpu.run())
      // stack should contain return address - 1 (so address of last byte in JSR)
      _ <- expect(cpu.memory.stackPopUShort(result.state)._2 == 0x0502).failFast
      // we end up with address + 1 after we terminate because we read the 0x00 (BRK) at address 0x060d
      _ <- expect(result.state.programCounter == 0x060e).failFast
    } yield success
  }

  simpleTest("RTS should set stack pointer and program counter correctly") {
    val program = Array(0x20, 0x06, 0x05, 0xa9, 0x05, 0x00, 0x60)
    val cpu = Cpu.load(program, 0x500)
    for {
      result <- IO(cpu.run())
      _ <- expect(result.state.stackPointer == 0xff).failFast
      _ <- expect(result.state.programCounter == 0x0506).failFast
    } yield success
  }

  simpleTest("STA should store a") {
    val program = Array(0xa9, 0xff, 0x85, 0xf0, 0x00)
    val cpu = Cpu.load(program)
    for {
      result <- IO(cpu.run())
      _ <- expect(result.state.a == 0xff).failFast
      _ <- expect(cpu.memory.read(0xf0) == 0xff).failFast
    } yield success
  }

  simpleTest("AND should modify a correctly") {
    val program = Array(0xa9, 0x0f, 0x29, 0x11, 0x00)
    val cpu = Cpu.load(program)
    for {
      result <- IO(cpu.run())
      _ <- expect(result.state.a == 0x01).failFast
    } yield success
  }

  simpleTest("CLC should clear carry flag") {
    val program = Array(0x18, 0x00)
    val cpu = Cpu.load(program)
    for {
      result <- IO(cpu.run(Some(CpuState(0, 0, 0, 0xff, 0x8000, CpuFlags.ValueSet(CpuFlags.Carry)))))
      _ <- expect(!result.state.flags.contains(CpuFlags.Carry)).failFast
    } yield success
  }

  simpleTest("ADC should set zero flag") {
    val program = Array(0x69, 0x00, 0x00)
    val cpu = Cpu.load(program)
    for {
      result <- IO(cpu.run(Some(CpuState(a = 0, 0, 0, 0xff, 0x8000, CpuFlags.ValueSet.empty))))
      _ <- expect(result.state.a == 0x00).failFast
      _ <- expect(result.state.flags.contains(CpuFlags.Zero)).failFast
    } yield success
  }

  simpleTest("ADC should set negative flag") {
    val program = Array(0x69, 0xff, 0x00)
    val cpu = Cpu.load(program)
    for {
      result <- IO(cpu.run(Some(CpuState(a = 0, 0, 0, 0xff, 0x8000, CpuFlags.ValueSet.empty))))
      _ <- expect(result.state.a == 0xff).failFast
      _ <- expect(result.state.flags.contains(CpuFlags.Negative)).failFast
    } yield success
  }

  simpleTest("ADC should set carry flag") {
    val program = Array(0x69, 0x80, 0x00)
    val cpu = Cpu.load(program)
    for {
      result <- IO(cpu.run(Some(CpuState(a = 0x81, 0, 0, 0xff, 0x8000, CpuFlags.ValueSet.empty))))
      _ <- expect(result.state.a == 0x01).failFast
      _ <- expect(result.state.flags.contains(CpuFlags.Carry)).failFast
    } yield success
  }

  simpleTest("ADC should set overflow flag") {
    val program = Array(0x69, 0x80, 0x00)
    val cpu = Cpu.load(program)
    for {
      result <- IO(cpu.run(Some(CpuState(a = 0x80, 0, 0, 0xff, 0x8000, CpuFlags.ValueSet.empty))))
      _ <- expect(result.state.a == 0x00).failFast
      _ <- expect(result.state.flags.contains(CpuFlags.Overflow)).failFast
    } yield success
  }

  simpleTest("CMP should set zero flag") {
    val program = Array(0xc9, 0x17, 0x00)
    val cpu = Cpu.load(program)
    for {
      result <- IO(cpu.run(Some(CpuState(a = 0x17, 0, 0, 0xff, 0x8000, CpuFlags.ValueSet.empty))))
      _ <- expect(result.state.a == 0x17).failFast
      _ <- expect(result.state.flags.contains(CpuFlags.Zero)).failFast
    } yield success
  }

  simpleTest("CMP should set negative flag") {
    val program = Array(0xc9, 0xff, 0x00)
    val cpu = Cpu.load(program)
    for {
      result <- IO(cpu.run(Some(CpuState(a = 0xf0, 0, 0, 0xff, 0x8000, CpuFlags.ValueSet.empty))))
      _ <- expect(result.state.a == 0xf0).failFast
      _ <- expect(result.state.flags.contains(CpuFlags.Negative)).failFast
    } yield success
  }

  simpleTest("CMP should set carry flag") {
    val program = Array(0xc9, 0x80, 0x00)
    val cpu = Cpu.load(program)
    for {
      result <- IO(cpu.run(Some(CpuState(a = 0x81, 0, 0, 0xff, 0x8000, CpuFlags.ValueSet.empty))))
      _ <- expect(result.state.a == 0x81).failFast
      _ <- expect(result.state.flags.contains(CpuFlags.Carry)).failFast
    } yield success
  }
}
