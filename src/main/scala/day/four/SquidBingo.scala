package day.four
import utils.FileUtils

class MarkedInt(val value: Int, val marked: Boolean = false)

class BingoBoard(val board: Array[Array[MarkedInt]]):

    def markNumber(n: Int): BingoBoard = 
        BingoBoard(board.map(row => 
            row.map(i => if i.value == n then MarkedInt(n, true) else i)))

    def isBingo: Boolean = hasCompleteRow || hasCompleteColumn

    def hasCompleteRow: Boolean = 
        board.exists {row => row.forall(_.marked == true)}
    
    def hasCompleteColumn: Boolean =
        BingoBoard(board.transpose).hasCompleteRow

    def unmarked: List[Int] = 
        board.toList.flatten
            .filter(_.marked == false)
            .map(_.value)

object SquidBingo:

    def getBingoBoards(input: List[String]): List[BingoBoard] =
        input.filter(!_.isEmpty)
            .grouped(5).toList
            .map(boardString =>
                    boardString.map(row => row.trim()
                                            .split(" +")
                                            .map(c => MarkedInt(c.toInt, false))
                                )
                                .toArray
                )
            .map(arr => new BingoBoard(arr))


    def getWinningBoard(randomNumbers: List[Int], boards: List[BingoBoard]): (BingoBoard, Int) = 
        if randomNumbers.isEmpty then throw Exception("No one wins")
        else 
            val markedBoards = boards.map(_.markNumber(randomNumbers.head))
            if markedBoards.exists(_.isBingo) then (markedBoards.filter(_.isBingo).head, randomNumbers.head)
            else getWinningBoard(randomNumbers.tail, markedBoards)

    def getPart1(input: List[String]): Int = 
        val randomNumbers = input(0).split(",").map(_.toInt).toList
        val boards = getBingoBoards(input.tail)
        val (winningBoard, finalValue) = getWinningBoard(randomNumbers, boards)
        getScore(winningBoard, finalValue)

    def getPart2(input: List[String]): Int = 
        val randomNumbers = input(0).split(",").map(_.toInt).toList
        val boards = getBingoBoards(input.tail)
        val (losingBoard, finalValue) = getLosingBoard(randomNumbers, boards)
        getScore(losingBoard, finalValue)

    def getScore(board: BingoBoard, lastNumber: Int) = 
        board.unmarked.sum * lastNumber

    def getLosingBoard(randomNumbers: List[Int], boards: List[BingoBoard]): (BingoBoard, Int) = 
        if randomNumbers.isEmpty then throw Exception("No one wins")
        else 
            val markedBoards = boards.map(_.markNumber(randomNumbers.head))
            if markedBoards.tail.isEmpty && markedBoards.forall(_.isBingo) then (markedBoards.head, randomNumbers.head)
            else getLosingBoard(randomNumbers.tail, markedBoards.filterNot(_.isBingo))
        

object Main extends App:
    val testInput = FileUtils.readListOfStringsFromTxt("src/main/scala/day/four/test-boards.txt")
    val input = FileUtils.readListOfStringsFromTxt("src/main/scala/day/four/boards.txt")
    println(s"Part 1 Example: ${SquidBingo.getPart1(testInput)}")
    println(s"Part 1 : ${SquidBingo.getPart1(input)}")
    println(s"Part 2 Example: ${SquidBingo.getPart2(testInput)}")
    println(s"Part 2 : ${SquidBingo.getPart2(input)}")
