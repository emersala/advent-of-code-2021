package day.one

import scala.annotation.tailrec
import utils.FileUtils

object SubmarineDepth:

    def countDeepening(depths: List[Int]): Int = 
        @tailrec
        def count(depths: List[Int], acc: Int): Int = 
            if (depths.isEmpty || depths.tail.isEmpty) 
                then acc
            else if (depths.head < depths.tail.head) 
                then count(depths.tail, acc + 1)
            else count(depths.tail, acc)

        count(depths, 0)

    def sumSlidingDepths(depths: List[Int]): List[Int] = 
        @tailrec
        def sum(depths: List[Int], acc: List[Int]): List[Int] = 
            if (depths.isEmpty || depths.tail.isEmpty || depths.tail.tail.isEmpty) 
                then acc
            else 
                val tripleSum = depths.head + depths.tail.head + depths.tail.tail.head
                sum(depths.tail, tripleSum::acc)

        sum(depths, Nil)

    // would be good to remove .reverse, this is the quickest reuse solution
    def countSliding(depths: List[Int]): Int =  countDeepening(sumSlidingDepths(depths).reverse)
     

object Main extends App:
    val depths = FileUtils.readListOfIntsFromTxt("/Users/sallyemerson/advent-of-code-21/src/main/scala/day/one/depths.txt")
    println(s"Day 1 Part 1: ${SubmarineDepth.countDeepening(depths)}")
    println(s"Day 2 Part 2: ${SubmarineDepth.countSliding(depths)}")
    

