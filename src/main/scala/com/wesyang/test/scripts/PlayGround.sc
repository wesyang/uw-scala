import com.persist.uw.examples.{State, Water}
import com.persist.uw.examples.WaterTest.Jar

val jars = List(Jar(3, 0, 0), Jar(5, 0, 4), Jar(8, 8, 4))
val sizes = State(jars.map(_.size))
val start = State(jars.map(_.start))
val end = State(jars.map(_.end))

val oldSet = Set[State]()
val w = Water (end, sizes)

val newStates = w.getNewStates(start)


val s2 = State (List(3, 0, 5))
val newStates2 = w.getNewStates{ s2 }

val set1 = Set (1, 2, 3,  6)
val set2 = Set (3, 4, 5, 6)
val set3 = Set [Int]()

set1.union( set2)
set3.union (set2)

w.solve(start)

