package day.five
import utils.FileUtils

object HydrothermalVentFinder:

    def isStraightLine(coords: Array[Array[Int]]) = 
        isCoordinate(coords) && (coords(0)(0) == coords(1)(0) || coords(0)(1) == coords(1)(1))

    def isCoordinate(coords: Array[Array[Int]]) =
        coords.length == 2 && coords(0).length == 2 && coords(1).length == 2

    def isValidLine(coords: Array[Array[Int]]) =
        isCoordinate(coords) && (coords(0)(0) == coords(1)(0) || coords(0)(1) == coords(1)(1) 
                || (coords(0)(1) - coords(1)(1))/(coords(0)(0) - coords(1)(0)) == 1)


    def straightLineCoordsToLineDef(coords: Array[Array[Int]]) : (x: Int, y: Int) => Boolean = 
        val start = coords(0)
        val end = coords(1)
        if start(0) == end(0)
        then 
            (x: Int,y: Int) => x == start(0) && y >= start(1).min(end(1)) && y <= start(1).max(end(1))
        else 
            (x: Int,y: Int) => y == start(1) && x >= start(0).min(end(0)) && x <= start(0).max(end(0))


    def lineToListCoords(coords: Array[Array[Int]]): List[(Int, Int)] =
        val x1 = coords(0)(0)
        val y1 = coords(0)(1)
        val x2 = coords(1)(0)
        val y2 = coords(1)(1)
        if x1 == x2
        then horizontalLine(x1, y1, y2).toList
        else if y1 ==y2 then verticalLine(y1, x1, x2).toList
        else if ((y2-y1)/(x2-x1)).abs == 1 then diagonalLine(x1, y1, x2, y2).toList
        else Nil

    def straightLineToListCoords(coords: Array[Array[Int]]): List[(Int, Int)] =
        val x1 = coords(0)(0)
        val y1 = coords(0)(1)
        val x2 = coords(1)(0)
        val y2 = coords(1)(1)
        if x1 == x2 // x are equal
        then horizontalLine(x1, y1, y2).toList
        else verticalLine(y1, x1, x2).toList

    def horizontalLine(x: Int, y1: Int, y2: Int) = 
        for (i <- y1.min(y2) until y1.max(y2) + 1)
            yield (x, i)

    def verticalLine(y: Int, x1: Int, x2: Int) = 
        for (i <- x1.min(x2) until x1.max(x2) + 1)
            yield (i, y)
    

    def diagonalLine(x1: Int, y1: Int, x2: Int, y2: Int) =
        val gradient = ((y2-y1)/(x2-x1))
        val intercept = y1 - gradient * x1
        for (i <- x1.min(x2) until x1.max(x2) + 1)
            yield (i, gradient * i + intercept)

    def countPart1AvoidPoints(vents: List[String]) = 
        vents.map(_.split(" -> ")
                    .map(_.split(",")
                    .map(_.toInt)))
                .filter(isStraightLine)
                .map(straightLineToListCoords)
                .flatten
                .foldLeft((Set.empty[(Int, Int)], Set.empty[(Int, Int)])){ case ((seen, duplicates), cur) => 
                    if(seen(cur)) (seen, duplicates + cur) else (seen + cur, duplicates)      
                    }._2
                .size

    def countPart2AvoidPoints(vents: List[String]) = 
        vents.map(_.split(" -> ")
                    .map(_.split(",")
                    .map(_.toInt)))
                .map(lineToListCoords)
                .flatten
                .foldLeft((Set.empty[(Int, Int)], Set.empty[(Int, Int)])){ case ((seen, duplicates), cur) => 
                    if(seen(cur)) (seen, duplicates + cur) else (seen + cur, duplicates)      
                    }._2
                .size

object Main extends App:
    val testVents = FileUtils.readListOfStringsFromTxt("src/main/scala/day/five/test-lengths.txt")
    val vents = FileUtils.readListOfStringsFromTxt("src/main/scala/day/five/vents.txt")
    println(s"Part 1 Test: ${HydrothermalVentFinder.countPart1AvoidPoints(testVents)}")
    println(s"Part 2 Test: ${HydrothermalVentFinder.countPart2AvoidPoints(testVents)}")

    println(s"Part 1 : ${HydrothermalVentFinder.countPart1AvoidPoints(vents)}")
    println(s"Part 2 : ${HydrothermalVentFinder.countPart2AvoidPoints(vents)}")