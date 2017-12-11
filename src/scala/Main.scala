package scala

object Main extends App {

  require(TwitterTask.solution_01(Array(1, 2, 3, 4, 5)) == 0)
  require(TwitterTask.solution_01(Array(1, 2, 5, 4, 5)) == 1)
  require(TwitterTask.solution_01(Array(5, 2, 5, 4, 5)) == 4)
  require(TwitterTask.solution_01(Array(5, 2, 5, 5, 5)) == 3)
  require(TwitterTask.solution_01(Array(5, 5, 5, 5, 5)) == 0)
  require(TwitterTask.solution_01(Array(5, 4, 3, 2, 1)) == 0)
  require(TwitterTask.solution_01(Array(5, 4, 3, 2, 5)) == 6)

}
