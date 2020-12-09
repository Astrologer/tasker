import cats.effect.{ConcurrentEffect, ExitCode, IO, IOApp}
import tasker.{Job, JobExecutionContext, JobRunnerBase, SumIntCable, Task}

class Tasks[F[_]: ConcurrentEffect] {
  val t1: Task[Int, Int, F] = Task.sync((x: Int) => x - 3, "sum")
  val t2: Task[Int, Int, F] = Task.sync((x: Int) => x + 2, "add")
  val t3: Task[Int, Int, F] = Task.sync((x: Int) => x * x, "mul")
}

object main extends IOApp {
  implicit val ex: JobExecutionContext[IO] = JobExecutionContext.io(1,1,1)
  val tasks = new Tasks[IO]
  import tasks._

  def run(args: List[String]): IO[ExitCode] = {
    val job = Job(t3, t1, JobRunnerBase, resultCable = SumIntCable)
      .dep(t3, t2)
      .dep(t2, t1)
      .dep(t3, t1)

    for {
      rJob <- job.launch(7)
      rTask <- t3.run(7)
      _ <- IO { println(s"ok = $rJob / $rTask") }
      _ <- IO { ex.terminate() }
    } yield ExitCode.Success
  }
}