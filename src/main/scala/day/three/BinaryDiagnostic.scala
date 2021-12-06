package day.three
import utils.FileUtils
import scala.annotation.tailrec



class BinaryDiagnoticRow(val arr: Array[Int]):

    def + (bdr: BinaryDiagnoticRow): BinaryDiagnoticRow =
        BinaryDiagnoticRow(arr.zip(bdr.arr).map(_ + _))

    def > (n: Int): BinaryDiagnoticRow =
        def f(x : Int) : Int = if x > n/2 then 1 else 0
        BinaryDiagnoticRow(arr.map(f))
    
    def inverse : BinaryDiagnoticRow =
        def inverse(i: Int): Int  = 
            i match
                case 0 => 1
                case 1 => 0

        BinaryDiagnoticRow(arr.map(inverse))
    
    def toInt : Int = 
        val l = arr.length
        def iter(arr: Array[Int], result: Int) : Int =
            if arr.isEmpty then result
            else 
                iter(arr.tail, result + (scala.math.pow(2, arr.length - 1) * arr.head).toInt )
        iter(arr, 0)

    override def toString : String =
        s"BinaryDiagnosticRow(${arr.mkString(", ")})"


object BinaryDiagnotic :

    def sumReport(report: List[BinaryDiagnoticRow]): BinaryDiagnoticRow = 
        val n = report.length
        @tailrec
        def sum(report: List[BinaryDiagnoticRow], acc:BinaryDiagnoticRow) : BinaryDiagnoticRow = 
            println(acc)
            if report.isEmpty then acc
            else sum(report.tail, acc + report.head)
        
        sum(report, BinaryDiagnoticRow(Array.fill(n)(0)))

    def getLength(report: List[BinaryDiagnoticRow]) = report.length

    def getGamma(report: List[BinaryDiagnoticRow]): BinaryDiagnoticRow = 
        val length = getLength(report)
        println(length)
        println(sumReport(report))
        val gamma = sumReport(report) > length
        println(gamma)
        gamma

    def getEpsilon(gamma: BinaryDiagnoticRow): BinaryDiagnoticRow = 
        println(gamma.inverse)
        gamma.inverse

    def getResult (report: List[BinaryDiagnoticRow]): Int = 
        val gamma : BinaryDiagnoticRow = this.getGamma(report)
        val epsilon : BinaryDiagnoticRow = gamma.inverse
        println(s"Gamma: $gamma, ${gamma.toInt}, Epilon: $epsilon , ${epsilon.toInt}")
        gamma.toInt * epsilon.toInt


object Main extends App:
    val report : List[BinaryDiagnoticRow] = FileUtils.readListOfStringsFromTxt("src/main/scala/day/three/report.txt")
                    .map(s => s.toCharArray.map(c => c.asDigit))
                    .map( arr => BinaryDiagnoticRow(arr))
    val testReport : List[BinaryDiagnoticRow] = FileUtils.readListOfStringsFromTxt("src/main/scala/day/three/test-report.txt")
                    .map(s => s.toCharArray.map(c => c.asDigit))
                    .map( arr => BinaryDiagnoticRow(arr))

    println(s"Day 3 Part 1: ${BinaryDiagnotic.getResult(report)}")
    //println(s"Day 3 Part 2 Test: ${BinaryDiagnotic.getLifeSupportRating(testReport)}")

   //println(s"Day 2 Part 2: ${SBinaryDiagnotic.getGamma(report).toInt)}")
    
