import Main._;

object Unparser {
  def main(array: Array[String]): Unit = {
    val x = MatrixImpl(10, 1000, "X")
    val y = MatrixImpl(10, 1000, "Y")
    val z = MatrixImpl(10, 1000, "Z")

    // (X+(Y+Z))*2
    print(unparse(ScalarOp(BinOp(x, MatrixSum, BinOp(y, MatrixSum, z)), 2)))
  }

  def unparse(x: Matrix): String = x match {
    case BinOp(x, op, y) =>
      val leftMatrix = unparse(x)
      val rightMatrix = unparse(y)
      op match {
        case MatrixSum => s"($leftMatrix+$rightMatrix)"
        case MatrixProduct => s"($leftMatrix$rightMatrix)"
      }
    case ScalarOp(x, y) =>
      s"(${unparse(x)}$y)"
    case MatrixImpl(_, _, id) => s"$id"
  }
}
