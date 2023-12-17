object Utils {
  final implicit class ListOps[T](l: List[T]) {
    def sumBy(f: T => Int): Int =
      l.foldLeft(0)(_ + f(_))

    def sumBy(f: T => Long): Long =
      l.foldLeft(0L)(_ + f(_))
  }

  final implicit class ArrayOps[T](l: Array[T]) {
    def sumBy(f: T => Int): Int =
      l.foldLeft(0)(_ + f(_))

    def sumBy(f: T => Long): Long =
      l.foldLeft(0L)(_ + f(_))
  }
}
