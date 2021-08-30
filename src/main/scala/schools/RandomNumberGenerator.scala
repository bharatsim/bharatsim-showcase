package schools

import java.util.SplittableRandom


case class RandomNumberGenerator() {
  val splittableRandom = new SplittableRandom()

  def nextDouble(): Double = {
    splittableRandom.nextDouble()
  }

  def nextInt(to: Int): Int = {
    splittableRandom.nextInt(to)
  }
}
