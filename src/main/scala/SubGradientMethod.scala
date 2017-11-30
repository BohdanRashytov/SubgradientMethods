import java.io.{BufferedWriter, FileWriter}
import java.lang.Math._

import SubgradientMethods._

class SubGradientMethod(X0: List[Double], stepRule: StepRule, fileName: String = "", epsilon: Double = 0.01) {

  private var X: List[List[Double]] = List(X0)
  private var step: Int = 0

  private val writer = new FileWriter(s"$fileName.txt")
  private val bufferWriter = new BufferedWriter(writer)

  bufferWriter.write("Iteration : F(X) \n")
  bufferWriter.write(s"${step} : ${f(X.last)} \n")
  bufferWriter.flush()

  private def iteration(): Boolean = {
    val current = X.last
    val dF = df(current)
    val alpha = stepRule.alpha(X.size, current)
    val nextX = current.zipWithIndex.map(xi => xi._1 - alpha*dF(xi._2))
    X = List(nextX)
    step += 1
    continue(nextX, epsilon) //&& step < 3*N
  }

  private def continue(x: List[Double], epsilon: Double) = sqrt(df(x).map(i => i*i).sum) > epsilon

  def run() = {
    while (iteration()) {
      bufferWriter.write(s"${step} : ${f(X.last)} \n")
      bufferWriter.flush()
    }
  }
}
