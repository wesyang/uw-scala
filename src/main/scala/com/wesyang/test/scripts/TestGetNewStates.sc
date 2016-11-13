import com.persist.uw.examples.State
import com.persist.uw.examples.WaterTest.Jar


val a:Int = 2
val b = 3
val c = a+b

val l1 = List(a, b, c)
val l2 = List(a, b, a+b)

l1 == l2
l1.equals (l2)

val s=State  (l1)
val s2=State (List(2, b, 2+b))
s == s2
s.equals(s2)

val jars = List(Jar(3, 0, 0), Jar(5, 1, 4), Jar(8, 8, 4))
val sizes = State(jars.map(_.size))
val start = State(jars.map(_.start))
val end = State(jars.map(_.end))

val p = sizes.contains zip start.contains

p.map (x=> x._1)
p.map (x=> x._2)
val sss = State (p.map(x=> x._2))

val pIdx = p zipWithIndex
val s5 = for {
  i <- pIdx;
  j <- pIdx
} yield (i, j)

val h = s5.head
val h1 = h._1
val h2 = h._2
h1._1._2


// remove puring water to the same busket
val s6 = s5.filter {case ((_, idx1), (_, idx2)) => idx1 != idx2 }
// remove empty source buket
val s7 = s6.filter { case (((_, source), _), _) => source >0 }
// remove full target
val s8 = s7.filter { case (_, ((size,target),_)) => size > target }

val s9= s8.map {
  case (((size1, source), idx1), ((size2, target), idx2))=>
    if (source > (size2-target))
      List(((size1, source-((size2-target))), idx1), ((size2, size2), idx2))
    else
      List (((size1, 0), idx1), ((size2, target+source), idx2))
}

val temp = s9.head.map {case (_, idx2) => idx2}.contains(1)
val f = pIdx.filter {
  case (_, idx) =>
    ! s9.head.map {case (_, idx2) => idx2}.contains(idx)
}

val s10 = s9.map (x => x :::
  pIdx.filter { case (__, idx) =>
    ! x.map {case (_, idx2) => idx2}.contains(idx)}
)

val s11 = s10.map (x => x.sortWith { case ((_, idx1), (_, idx2) ) => idx2 > idx1} )
start
val s12 = s11.map (x=> State (x.map {case ((size, cur),_)=> cur} ))
val result = (start, s12)


type Item = ((Int, Int), Int)
type SourceTarget = (Item, Item)
/*
def getNewItem (x: SourceTarget) : SourceTarget =
{
case (x, y) => (x, y)
}

val s9= s8.map (getNewItem )
*/



















