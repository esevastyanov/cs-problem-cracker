package training.hackerrank

/**
  * https://www.hackerrank.com/challenges/cloudy-day/problem
  */
object Cloudy_Day extends App
{
  Solution.main(args)

  object Solution
  {

    import scala.collection.mutable

    def maximumPeople(p: Array[Long], x: Array[Long], y: Array[Long], r: Array[Long]): Long = {
      val cities = x.zip(p).sorted
      val clouds = y.zip(r).map { case (yi, ri) => (yi - ri, yi + ri) }.sorted
      var city_i = 0
      var cloud_i = 0
      var populationUnderSun = 0L
      var maxPopulationUnderOneCloud = 0L
      val cb = mutable.ListBuffer[(Long, Long)]()

      def sumBufferedCitiesPopulation(): Unit = {
        val cloud = clouds(cloud_i)
        var populationUnderOnlyThatCloud = 0L
        while (cloud_i < clouds.length && isNextInside) {
          sumBeforeNextCloud()
          flushInsideNextCloud()
          cloud_i += 1
        }
        if (isNextToTheRight) {
          cb.foreach(populationUnderOnlyThatCloud += _._2)
        } else {
          sumBeforeNextCloud()
        }
        cb.clear()
        maxPopulationUnderOneCloud = math.max(maxPopulationUnderOneCloud, populationUnderOnlyThatCloud)

        def nextCloud = if (cloud_i + 1 < clouds.length) clouds(cloud_i + 1) else (Long.MaxValue, Long.MaxValue)

        def isNextToTheRight = cloud._2 < nextCloud._1

        def isNextInside: Boolean = cloud._1 <= nextCloud._1 && nextCloud._2 <= cloud._2

        def sumBeforeNextCloud(): Unit =
          while (cb.nonEmpty && cb.head._1 < nextCloud._1) {populationUnderOnlyThatCloud += cb.head._2; cb.remove(0)}

        def flushInsideNextCloud(): Unit = while (cb.nonEmpty && cb.head._1 <= nextCloud._2) {cb.remove(0)}
      }

      while (city_i < cities.length && cloud_i < clouds.length) {
        val city = cities(city_i)
        val cloud = clouds(cloud_i)
        if (city._1 < cloud._1) {
          populationUnderSun += city._2
          city_i += 1
        } else if (city._1 > cloud._2) {
          sumBufferedCitiesPopulation()
          cloud_i += 1
        } else {
          cb += city
          city_i += 1
        }
      }
      while (city_i < cities.length) {
        populationUnderSun += cities(city_i)._2
        city_i += 1
      }
      if (cb.nonEmpty) {
        sumBufferedCitiesPopulation()
      }
      populationUnderSun + maxPopulationUnderOneCloud
    }

    def main(args: Array[String]): Unit = {
      val sc = new java.util.Scanner(System.in)
      val n = sc.nextInt()
      val p = Array.fill(n)(sc.nextLong())
      val x = Array.fill(n)(sc.nextLong())
      val m = sc.nextInt()
      val y = Array.fill(m)(sc.nextLong())
      val r = Array.fill(m)(sc.nextLong())
      val result = maximumPeople(p, x, y, r)
      println(result)
    }
  }

}
