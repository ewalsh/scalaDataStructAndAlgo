package ai.economicdatasciences.arrays

object MatrixMultApp {
  def main(args: Array[String]): Unit = {
    val mat1: Array[Array[Double]] = Array(Array(2.5, 1.5, 0.5), Array(1, 2, 4))
    val mat2: Array[Array[Double]] =
      Array(Array(-1, 1.5, 1, -1), Array(0.5, -2, -2.5, 1), Array(1, 2, 1, 1))
    val resMat: Array[Array[Double]] = multiplyMatrices(mat1, mat2)

    // print results
    for (i <- 0 until resMat.length) {
      for (j <- 0 until resMat(i).length) {
        println(resMat(i)(j) + " ")
      }
      println()
    }
  }

  def multiplyMatrices(
      mat1: Array[Array[Double]],
      mat2: Array[Array[Double]]
  ): Array[Array[Double]] = {
    // preallocate
    val res: Array[Array[Double]] =
      Array.ofDim[Double](mat1.length, mat2(0).length)

    // get dimentions
    val mat1Rows = mat1.length
    val mat1Cols = mat1(0).length
    val mat2Rows = mat2.length
    val mat2Cols = mat2(0).length

    if (mat1Cols != mat2Rows) {
      sys.error(
        "Matrix 1 Columns: " + mat1Cols + " does not match with Matrix 2 rows: " + mat2Rows
      )
    } else {
      for (i <- 0 until mat1Rows) {
        for (j <- 0 until mat2Cols) {
          // for (k <- 0 until mat1Cols) {
          //   res(i)(j) += mat1(i)(k) * mat2(k)(j)
          // }
          res(i)(j) = mat1(i).zip(mat2.map(_(j))).map(x => x._1 * x._2).sum
        }
      }
      res
    }
  }
}
