package semu

import semu.model.CpuStatus
import weaver._

object CpuSuite extends SimpleIOSuite {
  simpleTest("LDA should set zero flag") {
    val program = Array(0xa9, 0x00, 0x00)
    val cpu = CPU.load(program)
    for {
      _ <- cpu.run()
      _ <- expect(cpu.registerA == 0).failFast
      _ <- expect(cpu.status.contains(CpuStatus.Zero)).failFast
    } yield success
  }

  simpleTest("LDA should set negative flag") {
    val program = Array(0xa9, 0xff, 0x00)
    val cpu = CPU.load(program)
    for {
      _ <- cpu.run()
      _ <- expect(cpu.registerA == 0xff).failFast
      _ <- expect(cpu.status.contains(CpuStatus.Negative)).failFast
    } yield success
  }
}
