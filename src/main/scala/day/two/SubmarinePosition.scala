package day.two
import utils.FileUtils

class Direction (val x: Int, val y: Int):  

    def +(d: Direction) : Direction = 
        Direction(this.x + d.x, this.y + d.y)

    def *(n: Int) : Direction = 
        Direction(n * this.x, n * this.y)


class DirectionAndAim(val direction: Direction, val aim: Int): 

  def move(e: Direction, n: Int) : DirectionAndAim = e match
    case Up => DirectionAndAim(this.direction, this.aim - n)
    case Down => DirectionAndAim(this.direction, this.aim + n)
    case Forward => this.moveForward(n)

  def moveForward(n: Int) : DirectionAndAim = 
    DirectionAndAim(Direction(this.direction.x + n, this.direction.y + this.aim * n), this.aim)

  
// TODO: should be able to implement more cleanly - need to research Scala

def getDirection(s: String): Direction = 
  if s.contains("forward") then Forward
  else if s == "down" then Down
  else if s == "up" then Up
  else throw Exception("No Match")


object Forward extends Direction(1,0)
object Down extends Direction(0,1)
object Up extends Direction(0,-1)

object SubmarinePosition {
  
    def getNetDirection(list : List[(Direction, Int)]): Direction =
      
      def iter(list: List[(Direction, Int)], acc: Direction): Direction = 
        if list.isEmpty then acc
        else iter(list.tail, acc + (list.head._1 * list.head._2))
    
      iter(list, Direction(0,0))
    
    def convertToDirectionList(list: List[String]): List[(Direction, Int)] = 
      list.map(s => 
        val ar = s.split(" ")
        println(ar(0))
        println(ar(1))
        (getDirection(ar(0)), ar(1).toInt))

    def getPosition(list: List[String]): Int = 
      val direction = getNetDirection(convertToDirectionList(list))
      direction.x * direction.y
}

// TODO: make more general so dont need to repeat for part 1 and part 2

object SubmarinePositionWithAim {
  
    def getNetDirectionAndAim(list : List[(Direction, Int)]): DirectionAndAim =
      
      def iter(list: List[(Direction, Int)], acc: DirectionAndAim): DirectionAndAim = 
        if list.isEmpty then acc
        else iter(list.tail, acc.move(list.head._1, list.head._2))
    
      iter(list, DirectionAndAim(Direction(0,0), 0))
    
    def convertToDirectionList(list: List[String]): List[(Direction, Int)] = 
      list.map(s => 
        val ar = s.split(" ")
        println(ar(0))
        println(ar(1))
        (getDirection(ar(0)), ar(1).toInt))

    def getPosition(list: List[String]): Int = 
      val direction = getNetDirectionAndAim(convertToDirectionList(list)).direction
      direction.x * direction.y
}

object Main extends App:
    val positions = FileUtils.readListOfStringsFromTxt("src/main/scala/day/two/directions.txt")
    println(s"Day 2 Part 1: ${SubmarinePosition.getPosition(positions)}}")
    println(s"Day 2 Part 2: ${SubmarinePositionWithAim.getPosition(positions)}")
    
