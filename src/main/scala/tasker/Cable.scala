package tasker

trait Cable[T, V] {
  def appendUnsafe(args: Any): V = append(args.asInstanceOf[T], None)
  def append(params: T, args: Option[V]): V
}

case object StringCable extends Cable[String, String] {
  def append(params: String, args: Option[String]): String = params
}

case object StringToIntCable extends Cable[String, Int] {
  def append(params: String, args: Option[Int]): Int = params.length
}

case object IntToIntCable extends Cable[Int, Int] {
  def append(params: Int, args: Option[Int]): Int = params
}

case object UnitCable extends Cable[Unit, Unit] {
  def append(params: Unit, args: Option[Unit]): Unit = ()
}

class IdentityCable[T] extends Cable[T, T] {
  def append(params: T, args: Option[T]): T = params
}