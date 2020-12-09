package tasker

trait Cable[T, V] {
  def appendUnsafe(params: Any, args: Option[Any]): Option[V] = append(params.asInstanceOf[T], args.asInstanceOf[Option[V]])
  def append(params: T, args: Option[V]): Option[V]
}

case object StringCable extends Cable[String, String] {
  def append(params: String, args: Option[String]): Option[String] = Some(params)
}

case object StringToIntCable extends Cable[String, Int] {
  def append(params: String, args: Option[Int]): Option[Int] = Some(params.length)
}

case object IntToIntCable extends Cable[Int, Int] {
  def append(params: Int, args: Option[Int]): Option[Int] = Some(params)
}

case object UnitCable extends Cable[Unit, Unit] {
  def append(params: Unit, args: Option[Unit]): Option[Unit] = Some(())
}

class IdentityCable[T] extends Cable[T, T] {
  def append(params: T, args: Option[T]): Option[T] = Some(params)
}

case object SumIntCable extends Cable[Int, Int] {
  def append(params: Int, args: Option[Int]): Option[Int] = Some(params + args.getOrElse(0))
}