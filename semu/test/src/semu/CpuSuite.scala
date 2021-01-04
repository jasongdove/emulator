package semu

import cats.effect.IO
import semu.model.CpuStatus
import weaver._

object CpuSuite extends SimpleIOSuite {
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
}
