package day.six

class Lanternfish(val internalTimer: Int):

    def dayPasses : List[Lanternfish] =
        if internalTimer > 0
        then List(Lanternfish(this.internalTimer - 1))
        else List(Lanternfish(8), Lanternfish(6))


object LanternfishCounter:
    
// part 1 - didn't work for 256 days
    def afterNDays(lanternFish: List[Lanternfish], n: Int) : Int = 
        if n == 0 then lanternFish.size
        else afterNDays(lanternFish.map(_.dayPasses).flatten, n-1)

// part 2 - need to be more clever
    def moreEfficientNDays(lanternFish: Array[Long], n: Int) : Long = 
        if n == 0 then lanternFish.sum
        else moreEfficientNDays(dayPasses(lanternFish), n-1)

    def toLanternFish(initialList: Array[Long]): Array[Long] = 
        (for (i <- 0 to 8) yield initialList.count(_ == i).toLong).toArray

    def dayPasses(fishCount: Array[Long]) : Array[Long] =
        (for (i<- 0 to 8) yield {
            if i != 6 && i!=8  then fishCount(i+1)
            else if i == 6 then fishCount(7) + fishCount(0)
            else fishCount(0)
        }).toArray

end LanternfishCounter



object Main extends App:
    import utils.FileUtils
    val testInitialState = LanternfishCounter.toLanternFish(Array(3,4,3,1,2))
    val initialState = FileUtils.readListOfStringsFromTxt("src/main/scala/day/six/fish.txt")(0)
                        .split(",")
                        .map(_.toLong)
    println(s"Day 6 Part 1 Test: ${LanternfishCounter.moreEfficientNDays(testInitialState, 80)}")
    println(s"Day 6 Part 1: ${LanternfishCounter.moreEfficientNDays(LanternfishCounter.toLanternFish(initialState), 80)}")
    println(s"Day 6 Part 2 Test: ${LanternfishCounter.moreEfficientNDays(testInitialState, 256)}")
    println(s"Day 6 Part 2: ${LanternfishCounter.moreEfficientNDays(LanternfishCounter.toLanternFish(initialState), 256)}")
