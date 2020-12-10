import cats.effect.{ConcurrentEffect, ExitCode, IO, IOApp}
import tasker.{Job, JobExecutionContext, JobRunnerBase, SumIntCable, Task}

class Tasks[F[_]: ConcurrentEffect] {
  val t1: Task[Int, Int, F] = Task.sync((x: Int) => x - 3, "sub")
  val t2: Task[Int, Int, F] = Task.sync((x: Int) => x + 2, "add")
  val t3: Task[Int, Int, F] = Task.sync((x: Int) => x * x, "mul")
}

object main extends IOApp {
  implicit val ex: JobExecutionContext[IO] = JobExecutionContext.io(1,1,1)
  val tasks = new Tasks[IO]
  import tasks._

  def run(args: List[String]): IO[ExitCode] = {
    val job = Job(t3, t1, JobRunnerBase, resultCable = SumIntCable, name = "job")
      .dep(t3, t2)
      .dep(t2, t1)
      .dep(t3, t1)

    val job2 = Job(t1, t2, JobRunnerBase)
      .dep(t1, job)
      .dep(job, t2)

    for {
      rTask <- t3.run(7)
      _ = println(s"Result: $rTask\n")

      rJob <- job.launch(7)
      _ = println(s"Result: $rJob\n")

      r <- job2.launch(10)
      _ = println(s"Result: $r\n")

      _ = ex.terminate()
    } yield ExitCode.Success
  }
}