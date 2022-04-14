object Main {
  sealed abstract class Matrix
  case class MatrixImpl(row: Int, column: Int, id: String) extends Matrix {
    def getRow: Int = {
      row
    }
    def getColumn: Int = {
      column
    }
  }

  sealed abstract class Operation
  sealed abstract class BinOperation extends Operation

  case object VectorSum extends BinOperation
  case object RowColumnProduct extends BinOperation
  case object OuterProduct extends BinOperation
  case object MatrixSum extends BinOperation
  case object MatrixProduct extends BinOperation

  case class BinOp(x: Matrix, op: BinOperation, y: Matrix) extends Matrix
  case class ScalarOp(x: Matrix, y: Int) extends Matrix


  def main(array: Array[String]): Unit = {
    val x = MatrixImpl(10, 1000, "X")
    val y = MatrixImpl(10, 1000, "Y")
    val z = MatrixImpl(10, 1000, "Z")

    // (X+(Y+Z))*2
    printAmountOfOperations(ScalarOp(BinOp(x, MatrixSum, BinOp(y, MatrixSum, z)), 2))

  }

  def printAmountOfOperations(x: Matrix): Unit = {
    println(calculate(x)._2)
  }


  def calculate(x: Matrix): (MatrixImpl, Int) = x match {
    case BinOp(x, op, y) =>
      val xMatrix = calculate(x)
      val yMatrix = calculate(y)

      // finding current amount of operations
      val currentAmountOfOperations = getPreviousCost(xMatrix, yMatrix)

      // matching the operand
      op match {
        case VectorSum =>
          // checking if the operation is legal
          val notSameAmountOfRows = xMatrix._1.getRow != yMatrix._1.getRow;
          if (notSameAmountOfRows) throw OperationError("The vectors does not have the same amount of rows, cannot calculate VectorSum", x)

          // getting amount of operations + previous operations
          val amountOfOperations = xMatrix._1.getColumn + currentAmountOfOperations
          (xMatrix._1, amountOfOperations)
        case RowColumnProduct => ???
        case OuterProduct => ???
        case MatrixSum =>
          // checking if the operation is legal
          val notSameAmountOfRows = xMatrix._1.getRow != yMatrix._1.getRow;
          val notSameAmountOfColumns = xMatrix._1.getColumn != yMatrix._1.getColumn;
          if (notSameAmountOfRows || notSameAmountOfColumns) throw OperationError("The matrices does not have the same dimensions, cannot calculate MatrixSum", x)

          // getting amount of operations + previous operations
          val row = xMatrix._1.getRow
          val column = xMatrix._1.getColumn
          val amountOfOperations = (row * column) + currentAmountOfOperations
          (xMatrix._1, amountOfOperations)
        case MatrixProduct =>
          // checking if the operation is legal
          val amountOfRowsInAMatrix = xMatrix._1.getRow        // m
          val amountOfColumnsInAMatrix = xMatrix._1.getColumn  // n
          val amountOfRowsInBMatrix = yMatrix._1.getRow        // n
          val amountOfColumnsInBMatrix = yMatrix._1.getColumn  // r

          val operationIsIllegal = amountOfColumnsInAMatrix != amountOfRowsInBMatrix
          if (operationIsIllegal) throw OperationError("The amount of columns in A does not equal amount of rows in B, cannot calculate MatrixProduct", x)

          val amountOfOperations = (2 * amountOfRowsInAMatrix * amountOfColumnsInAMatrix * amountOfColumnsInBMatrix) + currentAmountOfOperations
          (???, amountOfOperations)
      }
    case ScalarOp(x, _) =>
      val xMatrix = calculate(x)

      // finding current amount of operations
      val currentAmountOfOperations = xMatrix._2
      val amountOfOperations = (xMatrix._1.getRow * xMatrix._1.getColumn) + currentAmountOfOperations;

      (xMatrix._1, amountOfOperations)
    case MatrixImpl(row, column, id) =>
      (MatrixImpl(row, column, id), 0)
  }

  def getPreviousCost(x: (MatrixImpl, Int), y:(MatrixImpl, Int)): Int = {
    x._2 + y._2
  }

  case class OperationError(msg: String, matrix: Matrix) extends RuntimeException(s"Illegal operation: $msg at $matrix")
}
