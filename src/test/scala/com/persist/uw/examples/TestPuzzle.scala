package com.persist.uw.examples

import com.persist.uw.examples.WaterTest.Jar
import org.specs2._


class TestPuzzle extends mutable.Specification {

  def solve(name:String, sizes: State, start: State, end:State, debug:Boolean): Option[List[State]] = {
    val water = Water(end, sizes, debug)
    val result = water.solve(start)
    println(s"*** $name $sizes ***")
    result match {
      case None => println("No Solution")
      case Some(r) => for ((s, i) <- r.zipWithIndex) println(s"${i + 1} $s")
    }

    result
  }

  def test(name: String, jars: List[Jar], debug:Boolean = true): Option[List[State]] = {
    val sizes = State(jars.map(_.size))
    val start = State(jars.map(_.start))
    val end = State(jars.map(_.end))

    solve (name, sizes, start, end, debug)
  }

  def reverseTest (name: String, jars: List[Jar], debug:Boolean = true): Option[List[State]] = {
    val sizes = State(jars.map(_.size))
    val start = State(jars.map(_.start))
    val end = State(jars.map(_.end))

    // search from end to start
    //
    //  if there is a solution from <start> to <end>,
    //  it should also have a solutin from <end> to <start>
    solve (name, sizes, end, start, debug)
  }

  "BVT1" >> {
    val jars1 = List(Jar(3, 0, 0), Jar(5, 0, 4), Jar(8, 8, 4))
    val result =  test("BVT1", jars1, debug = false)  match {
      case Some(r) => r.size == 8
      case None => false
    }
    result shouldEqual(true)
  }

  "BVT2" >> {
    val jars1 = List(Jar(5, 0, 0), Jar(11, 0, 8), Jar(13, 0, 8), Jar(24, 24, 8))
    val result =  test("BVT2", jars1, debug = false)  match {
      case Some(r) => r.size == 7
      case None => false
    }
    result shouldEqual(true)
  }

  "BVT3" >> {
    val jars1 = List(Jar(4, 0, 3), Jar(5, 0, 3), Jar(10, 10, 4), Jar(10, 10, 10))
    val result =  test("BVT3", jars1, debug = false)  match {
      case Some(r) => r.size == 12
      case None => false
    }
    result shouldEqual(true)
  }

  "BVT3" >> {
    val jars1 = List(Jar(4, 0, 3), Jar(5, 0, 3), Jar(10, 10, 4), Jar(10, 10, 10))
    val result =  test("BVT3", jars1, debug = false)  match {
      case Some(r) => r.size == 12
      case None => false
    }
    result shouldEqual(true)
  }

  "BVT4" >> {
    val jars1 = List(Jar(2, 0, 2), Jar(2, 0, 2), Jar(100, 0, 48), Jar(100, 100, 48))
    val result =  test("BVT4", jars1, debug = false)  match {
      case Some(r) => r.size == 51
      case None => false
    }
    result shouldEqual(true)
  }

  "BVT5" >> {
    val jars1 = List(Jar(2, 0, 1), Jar(2, 0, 1), Jar(100, 0, 49), Jar(100, 100, 49))
    val result =  test("BVT5", jars1, debug = false)  match {
      case Some(r) => false
      case None => true
    }
    result shouldEqual(true)
  }

  "No Solution: all empty jars" >> {
    val jars1 = List(Jar(3, 0, 0), Jar(5, 0, 4), Jar(8, 0, 4))  // all empty jars
    val result =  test("all empty jars\"", jars1, debug = false)  match {
      case Some(r) => false
      case None => true
    }
    result shouldEqual(true)
  }

  "No Solution: all jars full" >> {
    val jars1 = List(Jar(3, 3, 1), Jar(5, 5, 4), Jar(8, 8, 4))
    val result =  test("all jars full", jars1, debug = false)  match {
      case Some(r) => false
      case None => true
    }
    result shouldEqual(true)
  }

  "No Solution: no enough water" >> {
    val jars1 = List(Jar(3, 3, 0), Jar(5, 5, 1), Jar(8, 0, 8))
    val result = test("no enough water", jars1, debug = false) match {
      case Some(r) => false
      case None => true
    }
    result shouldEqual (true)
  }

  "No Solution:  water shortage" >> {
    val jars1 = List(Jar(3, 2, 3), Jar(5, 4, 5), Jar(8, 6, 8))
    val result = test(" water shortage", jars1, debug = false) match {
      case Some(r) => false
      case None => true
    }
    result shouldEqual (true)
  }

  "No Solution:   too much water" >> {
    val jars1 = List(Jar(3, 2, 1), Jar(5, 4, 2), Jar(8, 6, 5))
    val result = test("  too much water", jars1, debug = false) match {
      case Some(r) => false
      case None => true
    }
    result shouldEqual (true)
  }

  "<start> - <end> are the same state" >> {
    val jars1 = List(Jar(3, 0, 0), Jar(5, 0, 0), Jar(8, 8, 8))
    val result = test("  start - end are the same", jars1, debug = false) match {
      case Some(r) => r.size == 1
      case None => false
    }
    result shouldEqual (true)
  }

  "just need search 1 level down to find solution" >> {
    val jars1 = List(Jar(3, 0, 0), Jar(5, 0, 5), Jar(8, 8, 3))
    val result = test("  just need search 1 level down", jars1, debug = false) match {
      case Some(r) => r.size == 2
      case None => false
    }
    result shouldEqual (true)
  }

  "search two level down to find solution" >> {
    val jars1 = List(Jar(3, 0, 3), Jar(5, 0, 2), Jar(8, 8, 3))
    val result = test(" search two level down", jars1, debug = false) match {
      case Some(r) => r.size == 3
      case None => false
    }
    result shouldEqual (true)
  }

  "if there is a solution from <start> to <end>, it must also have solution from <end> to <start>" >> {
    val jars1 = List(Jar(5, 0, 0), Jar(11, 0, 8), Jar(13, 0, 8), Jar(24, 24, 8))
    val result = test("search from start to end", jars1, debug = false) match {
      case Some(r) => {
          reverseTest("reverses search from end to start", jars1, false) match {
            case Some(rr) => true
            case None => true
          }
      }
      case None => false
    }
    result shouldEqual (true)
  }
}


