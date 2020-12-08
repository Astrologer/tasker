package tasker

import java.util.concurrent.{ExecutorService, Executors}

import cats.effect.{ConcurrentEffect, ContextShift, IO}

import scala.concurrent.ExecutionContext

case class JobExecutionContext[F[_]](
  baseCE: ConcurrentEffect[F],
  baseCS: ContextShift[F],
  workCS: ContextShift[F],
  ioCS: ContextShift[F],
  x1: ExecutorService,
  x2: ExecutorService,
  x3: ExecutorService
) {
  def terminate(): Unit = {
    x1.shutdown()
    x2.shutdown()
    x3.shutdown()
  }
}

object JobExecutionContext {
  def io(csBaseSize: Int, csWorkSize: Int, csIOSize: Int): JobExecutionContext[IO] = {
    val baseEx: ExecutorService = Executors.newFixedThreadPool(csBaseSize)
    val workEx = Executors.newFixedThreadPool(csWorkSize)
    val ioEx = Executors.newFixedThreadPool(csIOSize)

    val baseEc = ExecutionContext.fromExecutor(baseEx)
    val workEc = ExecutionContext.fromExecutor(workEx)
    val ioEc = ExecutionContext.fromExecutor(ioEx)

    val baseCs = IO.contextShift(baseEc)
    val workCs = IO.contextShift(workEc)
    val ioCs = IO.contextShift(ioEc)

    JobExecutionContext(IO.ioConcurrentEffect(baseCs), baseCs, workCs, ioCs, baseEx, workEx, ioEx)
  }
}