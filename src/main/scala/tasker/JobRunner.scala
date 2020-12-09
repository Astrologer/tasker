package tasker

import cats.effect.ConcurrentEffect
import cats.effect.concurrent.Deferred
import cats.syntax.flatMap._
import cats.syntax.functor._
import cats.syntax.traverse._
import fs2.concurrent.{Queue, SignallingRef}

trait JobRunner {
  def execute[T, V, F[_]: JobExecutionContext](job: JobBase[T, V, F], args: T): F[V]
}

object JobRunnerBase extends JobRunner {

  private def runTask[F[_]: JobExecutionContext](task: TaskBase[_, _, F], args: Any, queue: Queue[F, ((String, String), Any)]) = {
    // TODO
    //  - get context by task type
    //  - execution timeout
    implicit val F: ConcurrentEffect[F] = implicitly[JobExecutionContext[F]].baseCE
    F.start(task.runEx(args).flatMap(result => queue.enqueue1((task.id, task.name) -> result)))
  }

  override def execute[T, V, F[_]: JobExecutionContext](job: JobBase[T, V, F], args: T): F[V] = {
    implicit val F: ConcurrentEffect[F] = implicitly[JobExecutionContext[F]].baseCE
    val firstTask = job.firstTask.instance
    val jobState = JobState().launched(firstTask.id, args)

    for {
      // TODO _ <- sc.shift to base poll
      jobResult <- Deferred[F, V]
      signal <- SignallingRef[F, Boolean](false)
      queue <- Queue.bounded[F, ((String, String), Any)](10)

      _ <- runTask(firstTask.asInstanceOf[Task[_, _, F]], args, queue)
      _ <- queue
       .dequeue
       .evalMapAccumulate(jobState) { case (currentState, ((id, name), result)) =>
         println(s" $name:$id = $result")
         val tasks = job.getDeps(name)
         val state = currentState
           .finished(id)
           .launched(tasks.map(_.id), result)
           .mapResult(r => if (name == job.lastTask.name) job.resultCable.appendUnsafe(result, r) else r)

         if (state.isEmpty)
           jobResult
             .complete(state.getResult)
             .flatMap(_ => signal.set(true))
             .map(_ => state -> ())
         else {
           tasks
             .map( runTask[F](_, result, queue))
             .sequence
             .map(_ => state -> ())
         }
       }
       .interruptWhen(signal)
       .compile
       .drain

      output <- jobResult.get
    } yield output
  }

  case class JobState(running: List[(String, Any)] = Nil, result: Option[Any] = None) {
    def finished(name: String): JobState = this.copy(running = running.filterNot(_._1 == name))
    def launched(name: String, args: Any): JobState = this.copy(running = running :+ name -> args)
    def launched(names: List[String], args: Any): JobState = this.copy(running = running ++ names.map(_ -> args))
    def mapResult(f: Option[Any] => Option[Any]): JobState = this.copy(result = f(result))
    def isEmpty: Boolean = running.isEmpty
    def getResult[V]: V = result.map(_.asInstanceOf[V]).getOrElse(throw new Exception("Unexpected state"))
  }

  implicit class JobBaseOps[F[_]](job: JobBase[_, _, F]) {
    def getDeps(name: String): List[TaskBase[_, _, F]] =
      job
        .deps
        .filter(_.from.name == name)
        .map(_.to)
        .map(_.instance)
  }
}