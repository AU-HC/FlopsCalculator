import Unparser.unparse

object Main {
  val locale = new java.util.Locale("de", "DE");
  val formatter = java.text.NumberFormat.getIntegerInstance(locale)

  sealed abstract class Matrix
  case class MatrixImpl(row: Int, column: Int, id: String) extends Matrix {
    def getRow: Int = {
      row
    }
    def getColumn: Int = {
      column
    }
    def getId: String = {
      id
    }
  }

  sealed abstract class Operation
  sealed abstract class BinOperation extends Operation

  case object MatrixSum extends BinOperation
  case object MatrixProduct extends BinOperation

  case class BinOp(x: Matrix, op: BinOperation, y: Matrix) extends Matrix
  case class ScalarOp(x: Matrix, y: Int) extends Matrix


  def main(array: Array[String]): Unit = {
    val a = MatrixImpl(5, 1000, "A")
    val b = MatrixImpl(1000, 200, "B")
    val c = MatrixImpl(200, 10, "C")

    printAmountOfOperations(BinOp(a, MatrixProduct, BinOp(b, MatrixProduct, c)))
    printAmountOfOperations(BinOp(BinOp(a, MatrixProduct, b), MatrixProduct, c))
  }

  def printAmountOfOperations(x: Matrix): Unit = {
    println(s"The following expression has been evaluated: ${unparse(x)}")

    try {
      val evaluated = calculate(x)
      println(s"Total amount of flops used: \n" +
        s"${evaluated._3} = ${formatter.format(evaluated._2)}")
    }
    catch {
      case OperationError(msg, matrix) => println(s"$msg at matrix $matrix")
      case _ => println("unknown error")
    }

    println("----------------------------------------------------------------------")
  }


  def calculate(x: Matrix): (MatrixImpl, Int, String) = x match {
    case BinOp(leftMatrix, op, rightMatrix) =>
      val xMatrix = calculate(leftMatrix)
      val yMatrix = calculate(rightMatrix)

      // finding current amount of operations
      val currentAmountOfOperations = getPreviousCost(xMatrix, yMatrix)

      // matching the operand
      op match {
        case MatrixSum =>
          // checking if the operation is legal
          val notSameAmountOfRows = xMatrix._1.getRow != yMatrix._1.getRow;
          val notSameAmountOfColumns = xMatrix._1.getColumn != yMatrix._1.getColumn;
          if (notSameAmountOfRows || notSameAmountOfColumns) throw OperationError("The matrices does not have the same dimensions, cannot calculate MatrixSum", x)

          // getting amount of operations + previous operations
          val row = xMatrix._1.getRow
          val column = xMatrix._1.getColumn
          val amountOfOperations = (row * column) + currentAmountOfOperations
          (xMatrix._1, amountOfOperations, s"$row * $column + ${xMatrix._3} +${yMatrix._3}")
        case MatrixProduct =>
          // checking if the operation is legal
          val amountOfRowsInAMatrix = xMatrix._1.getRow        // m
          val amountOfColumnsInAMatrix = xMatrix._1.getColumn  // n
          val amountOfRowsInBMatrix = yMatrix._1.getRow        // n
          val amountOfColumnsInBMatrix = yMatrix._1.getColumn  // r

          val operationIsIllegal = amountOfColumnsInAMatrix != amountOfRowsInBMatrix
          if (operationIsIllegal) throw OperationError("The amount of columns in A does not equal amount of rows in B, cannot calculate MatrixProduct", x)

          val amountOfOperations = (2 * amountOfRowsInAMatrix * amountOfColumnsInAMatrix * amountOfColumnsInBMatrix) + currentAmountOfOperations

          (MatrixImpl(amountOfRowsInAMatrix, amountOfColumnsInBMatrix, ""), amountOfOperations, s"(2 * $amountOfRowsInAMatrix * $amountOfColumnsInAMatrix * $amountOfColumnsInBMatrix) + ${xMatrix._3} + ${yMatrix._3}")
      }
    case ScalarOp(x, _) =>
      val xMatrix = calculate(x)

      // finding current amount of operations
      val currentAmountOfOperations = xMatrix._2
      val amountOfOperations = (xMatrix._1.getRow * xMatrix._1.getColumn) + currentAmountOfOperations;
      val currentAmountOfOperationsString = if (currentAmountOfOperations == 0) "" else s"+${xMatrix._3}"

      (xMatrix._1, amountOfOperations, s"(${xMatrix._1.getRow} * ${xMatrix._1.getColumn}) + $currentAmountOfOperations")
    case MatrixImpl(row, column, id) =>
      (MatrixImpl(row, column, id), 0, "0")
  }

  def getPreviousCost(x: (MatrixImpl, Int, String), y:(MatrixImpl, Int, String)): Int = {
    x._2 + y._2
  }

  case class OperationError(msg: String, matrix: Matrix) extends RuntimeException(s"Illegal operation: $msg at $matrix")
}
