package com.persist.uw.examples

case class State(contains: List[Int]) {
  override def toString = contains.mkString("[", ",", "]")
  def itemCount ():Int = contains.size
}

case class Water(end: State, sizes: State, debug:Boolean=false) {
  type Path = List[State]
  type StateGroup = (State, List[State])

  def debug_print(old: Set[State], childGroup: Set[StateGroup], newSet: Set[State], indent: Int) = {
    if (debug) {
      println("-----------------step (" + indent.toString() + ") -------------------- ");
      println("   old set " + old.toString())
      println("   child group " + childGroup.toString())
      println("   childs " + newSet.toString())
      println()
    }
  }

  def getNewStates (current: State): StateGroup = {
    // combine size and current state then index them
    val ssIdx = (sizes.contains zip current.contains).zipWithIndex

    // step1: find out how many combination to generate child states
    val step1 = (for {i <- ssIdx; j <- ssIdx} yield (i, j))
      .filter { case ((_, idx1), (_, idx2)) => idx1 != idx2 } // filter source & target are the same jar
      .filter { case (((_, source), _), _) => source > 0 } // filter source jar which is empty
      .filter { case (_, ((size, target), _)) => size > target } // filter target jar is already full

    // step2: produce new state (only source jar and target jar)
    val step2 = step1.map {
      case (((size1, source), idx1), ((size2, target), idx2)) =>
        if (source > (size2 - target))
          List(((size1, source - ((size2 - target))), idx1), ((size2, size2), idx2))
        else
          List(((size1, 0), idx1), ((size2, target + source), idx2))
    }

    // step3: add unchanged jar back to the list
    val step3 = (step2.map(x => x ::: ssIdx.filter { case (__, idx) => !x.map { case (_, idx2) => idx2 }.contains(idx) })
      .map(x => x.sortWith { case ((_, idx1), (_, idx2)) => idx2 > idx1 })
      )

    // step4 convert list back to State object
    val newStates = step3.map(x => State(x.map { case ((size, cur), _) => cur }))

    // group current state with new child states
    (current, newStates)
  }



  def solve(start: State): Option[Path] = {
    def getParent (current:State, newStateGroups:Set[StateGroup]) : State =
    {
      newStateGroups.filter {case (parent, childs) => childs.contains(current)}.head
      match {case ( parent, _) => parent}
    }

    def solveAction(old: Set[State], current: Set[State], step:Int): Option[Path] = {
      val newStateGroups = current.map(x => getNewStates((x)))
      val newOldSet = old.union(current)
      val childSet = (newStateGroups.flatMap { case (_, states) => states })
        .filter(x => !newOldSet.contains(x))  // filter state which has been seen

      debug_print(newOldSet, newStateGroups, childSet, step);

      if (childSet.isEmpty) None  // no solution
      else if (childSet.contains(end)) {  // find solution
        val result = List(end)
        Option(getParent (end, newStateGroups) :: result)
      }
      else {
        solveAction (newOldSet, childSet, step+1) match {
          case Some (path) => Option(getParent(path.head, newStateGroups) :: path)
          case None => None
        }
      }
    }

    if (start.itemCount() != end.itemCount()) throw new IllegalArgumentException ("mis-match itemCount between start & end")
    if (start.itemCount() != sizes.itemCount()) throw new IllegalArgumentException ("mis-match itemCount between start & sizes")

    if (start == end) Option(List(start))
    else {
      solveAction(Set[State](), Set[State](start), 0)
    }
  }

}

object WaterTest {

  case class Jar(size: Int, start: Int, end: Int)

  def test(name: String, jars: List[Jar]): Unit = {
    val sizes = State(jars.map(_.size))
    val start = State(jars.map(_.start))
    val end = State(jars.map(_.end))
    val water = Water(end, sizes)
    val result = water.solve(start)
    println(s"*** $name $sizes ***")
    result match {
      case None => println("No Solution")
      case Some(r) =>
        if (r.size > 25) {
          println(s"Too many steps ${r.size}")
        } else
          for ((s, i) <- r.zipWithIndex) println(s"${i + 1} $s")
    }
  }

  def main(args: Array[String]): Unit = {
    val jars1 = List(Jar(3, 0, 0), Jar(5, 0, 4), Jar(8, 8, 4))
    val jars2 = List(Jar(5, 0, 0), Jar(11, 0, 8), Jar(13, 0, 8), Jar(24, 24, 8))
    val jars3 = List(Jar(4, 0, 3), Jar(5, 0, 3), Jar(10, 10, 4), Jar(10, 10, 10))
    val jars4 = List(Jar(2, 0, 2), Jar(2, 0, 2), Jar(100, 0, 48), Jar(100, 100, 48))
    val jars5 = List(Jar(2, 0, 1), Jar(2, 0, 1), Jar(100, 0, 49), Jar(100, 100, 49))
    test("Test1", jars1)
    test("Test2", jars2)
    test("Test3", jars3)
    test("Test4", jars4)
    test("Test5", jars5)
  }
}
