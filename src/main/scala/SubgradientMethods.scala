import java.lang.Math._

object SubgradientMethods {

  val N = 10000
  val M = 9000
  val mu = 1.0

  val point: List[Double] = (1 to N).map(_ => random()).toList

  def f(x: List[Double]): Double = x.take(M).max + mu / 2 * x.map(i => i*i).sum

  def df(x: List[Double]): List[Double] = {
    val max = x.take(M).max
    val i = x.indexWhere(_ == max)
    x.zipWithIndex.map(d => d._1 * mu + (if (d._2 == i) 1.0 else 0.0))
  }

  def main(args: Array[String]): Unit = {
    new SubGradientMethod(point, ConstantStepSize(0.001), "SubGradientMethod-ConstantStepSize").run()
    new SubGradientMethod(point, ConstantStepLength(0.001), "SubGradientMethod-ConstantStepLength").run()
    new SubGradientMethod(point, SquareSummableButNotSummable(1.0, 1.0), "SubGradientMethod-SquareSummableButNotSummable").run()
    new SubGradientMethod(point, NonSummableDiminishing(1.0), "SubGradientMethod-NonSummableDiminishing").run()
  }

}
