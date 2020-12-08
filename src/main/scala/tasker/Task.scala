package tasker

import java.util.UUID
import cats.effect.Sync

trait TaskBase[T, V, F[_]] {
  val name: String
  def run: T => F[V]

  private[tasker] def runEx(args: Any)(implicit ex: JobExecutionContext[F]): F[V]
}

case class Task[T, V, F[_]](run: T => F[V], name: String = UUID.randomUUID().toString) extends TaskBase[T, V, F] {
  private[tasker] def runEx(args: Any)(implicit ex: JobExecutionContext[F]): F[V] = run(args.asInstanceOf[T])
}

object Task {
  def sync[T, V, F[_]: Sync](run: T => V): Task[T, V, F] =
    Task( (x: T) => Sync[F].delay(run(x)))
}
