object Utils {
  final implicit class ListOps[T](l: List[T]) {
    def sumBy(f: T => Int): Int =
      l.foldLeft(0)(_ + f(_))
  }
}
