package tasker

import java.util.UUID

case class Dependency[T, V, F[_]](from: TaskBase[_, T, F], to: TaskBase[V, _, F], appender: Cable[T, V])

trait JobBase[T, V, F[_]] extends TaskBase[T, V, F] {
  val firstTask: TaskBase[T, _, F]
  val lastTask: TaskBase[_, V, F]
  val deps: List[Dependency[_, _, F]]
  val runner: JobRunner

  def launch(args: T)(implicit ex: JobExecutionContext[F]): F[V]
}

case class Job[T, V, F[_]](
    firstTask: TaskBase[T, _, F],
    lastTask: TaskBase[_, V, F],
    runner: JobRunner,
    deps: List[Dependency[_, _, F]] = Nil,
    name: String = UUID.randomUUID().toString) extends JobBase[T, V, F] {

  def run: T => F[V] = ???
  def launch(args: T)(implicit ex: JobExecutionContext[F]): F[V] = runner.execute(this, args)(ex)
  override private[tasker] def runEx(args: Any)(implicit ex: JobExecutionContext[F]): F[V] = runner.execute(this, args.asInstanceOf[T])(ex)

  def dep[A, B](from: TaskBase[_, A, F], to: TaskBase[B, _, F], cable: Cable[A, B]): Job[T, V, F] =
    this.copy(deps = deps :+ Dependency(from, to, cable))

  def dep[A](from: TaskBase[_, A, F], to: TaskBase[A, _, F]): Job[T, V, F] =
    this.copy(deps = deps :+ Dependency(from, to, new IdentityCable[A]))
}