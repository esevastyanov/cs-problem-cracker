package training.other

/**
  * https://habrahabr.ru/post/200190/
  **/
object TwitterTask {

  def solution_01(land: Array[Int]): Int = {
    var leftMax = 0
    var rightMax = 0
    var left = 0
    var right = land.length - 1
    var volume = 0
    while (left < right) {
      if (land(left) > leftMax) {
        leftMax = land(left)
      }
      if (land(right) > rightMax) {
        rightMax = land(right)
      }
      if (leftMax >= rightMax) {
        volume += rightMax - land(right)
        right -= 1
      } else {
        volume += leftMax - land(left)
        left += 1
      }
    }
    volume
  }
}
