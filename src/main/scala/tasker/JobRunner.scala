package tasker

import cats.effect.ConcurrentEffect
import cats.effect.concurrent.Deferred
import cats.syntax.flatMap._
import cats.syntax.functor._
import fs2.Stream
import fs2.concurrent.{Queue, SignallingRef}

trait JobRunner {
  def execute[T, V, F[_]: JobExecutionContext](job: JobBase[T, V, F], args: T): F[V]
}

object JobRunnerBase extends JobRunner {

  private def runTask[F[_]: JobExecutionContext](task: Task[_, _, F], args: Any, queue: Queue[F, (String, Any)]) = {
    // TODO get context by task type
    implicit val F: ConcurrentEffect[F] = implicitly[JobExecutionContext[F]].baseCE
    F.start(task.runEx(args).flatMap(result => queue.enqueue1(task.name -> result)))
  }

  override def execute[T, V, F[_]: JobExecutionContext](job: JobBase[T, V, F], args: T): F[V] = {
    implicit val F: ConcurrentEffect[F] = implicitly[JobExecutionContext[F]].baseCE

    for {
      jobResult <- Deferred[F, V]
      signal <- SignallingRef[F, Boolean](false)
      queue <- Queue.bounded[F, (String, Any)](10)

      _ <- runTask(job.firstTask.asInstanceOf[Task[_, _, F]], args, queue)
      _ <- queue
       .dequeue
       .flatMap { case (_, result) =>
         /* context.update(taskName, result)
          * val toRun = context.readyTasks
          *
          */
         Stream.eval(jobResult.complete(result.asInstanceOf[V]))
           .evalMap(_ => signal.set(true))
       }
       .interruptWhen(signal)
       .compile
       .drain

      output <- jobResult.get
    } yield output
  }
}
