package day.two
import utils.FileUtils

class Direction (val x: Int, val y: Int):  

    def +(d: Direction) : Direction = 
        Direction(this.x + d.x, this.y + d.y)

    def *(n: Int) : Direction = 
        Direction(n * this.x, n * this.y)

    def move(e: Direction, n: Int) : Direction = this + (e * n)

object Forward extends Direction(1,0)
object Down extends Direction(0,1)
object Up extends Direction(0,-1)

def getDirection(s: String): Direction = s match
  case "forward" => Forward
  case "down" => Down
  case "up" => Up


trait SubmarinePosition {
  
    def getNetDirection(list : List[(Direction, Int)]): Direction
    
    def convertToDirectionList(list: List[String]): List[(Direction, Int)] = 
      list.map(s => 
        val ar = s.split(" ")
        (getDirection(ar(0)), ar(1).toInt))

    def getPosition(list: List[String]): Int = 
      val direction = getNetDirection(convertToDirectionList(list))
      direction.x * direction.y
}

object SubmarinePositionWithoutAim extends SubmarinePosition {
  def getNetDirection(list : List[(Direction, Int)]): Direction =
      
      def iter(list: List[(Direction, Int)], acc: Direction): Direction = 
        if list.isEmpty then acc
        else iter(list.tail, acc.move(list.head._1, list.head._2))
    
      iter(list, Direction(0,0))
}

// extend for part 2

class DirectionAndAim(val direction: Direction, val aim: Int): 

  def move(e: Direction, n: Int) : DirectionAndAim = e match
    case Up => DirectionAndAim(this.direction, this.aim - n)
    case Down => DirectionAndAim(this.direction, this.aim + n)
    case Forward => this.moveForward(n)

  def moveForward(n: Int) : DirectionAndAim = 
    DirectionAndAim(Direction(this.direction.x + n, this.direction.y + this.aim * n), this.aim)


object SubmarinePositionWithAim extends SubmarinePosition{
  
    def getNetDirection(list : List[(Direction, Int)]): Direction =
      
      def iter(list: List[(Direction, Int)], acc: DirectionAndAim): DirectionAndAim = 
        if list.isEmpty then acc
        else iter(list.tail, acc.move(list.head._1, list.head._2))
    
      iter(list, DirectionAndAim(Direction(0,0), 0)).direction

}

object Main extends App:
    val positions = FileUtils.readListOfStringsFromTxt("src/main/scala/day/two/directions.txt")
    println(s"Day 2 Part 1: ${SubmarinePositionWithoutAim.getPosition(positions)}")
    println(s"Day 2 Part 2: ${SubmarinePositionWithAim.getPosition(positions)}")
    
