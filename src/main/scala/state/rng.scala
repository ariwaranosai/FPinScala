package state

/**
  * Created by ariwaranosai on 16/7/9.
  *
  */

trait RNG {
  def next: (Int, RNG)
}
