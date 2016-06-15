import datastructures._
import List._


val t = List(1,2,3,4)

List.sum(t)

foldLeft(t, 0)(_ + _)
foldRight(t, 0)(_ + _)
length(t)

map_1(t)(_ + 1)

zipWith(t, t)((_, _))
