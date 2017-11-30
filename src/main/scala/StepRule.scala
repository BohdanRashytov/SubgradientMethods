import java.lang.Math.sqrt

import SubgradientMethods._

sealed trait StepRule {
  def alpha(k: Int = 0, point: List[Double] = List()): Double
}

case class ConstantStepSize(h: Double) extends StepRule {
  override def alpha(k: Int = 0, point: List[Double] = List()): Double = h
}

case class ConstantStepLength(h: Double) extends StepRule {
  override def alpha(k: Int, x: List[Double] = List()): Double = h / sqrt(df(x).map(i => i*i).sum)
}

case class SquareSummableButNotSummable(a: Double, b: Double) extends StepRule{
  override def alpha(k: Int = 0, point: List[Double] = List()): Double = a / (b + k)
}

case class NonSummableDiminishing(a: Double) extends StepRule{
  override def alpha(k: Int = 0, point: List[Double] = List()): Double = a / sqrt(k + 1)
}