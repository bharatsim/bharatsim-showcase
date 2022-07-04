package multipleStrains

import org.apache.commons.math3.distribution.ExponentialDistribution


/**
 * Utility to create exponential distribution
 *
 * @param mean mean of the exponential distribution
 * @return Exponential distribution with given mean
 */
case class Exponential(mean: Double) {

  private val dist: ExponentialDistribution = new ExponentialDistribution(mean)

  /**
   * Draw random sample from the exponential distribution
   *
   * @return random sample
   */
  def sample(): Double = {
    dist.sample()
  }

  /**
   * Draw random sample from the exponential distribution
   *
   * @param size number of random samples to be returned
   * @return random sample of specific size
   */
  def sample(size: Int): Array[Double] = {
    if (size <= 0) {
      return Array.empty[Double]
    }
    dist.sample(size)
  }

}
