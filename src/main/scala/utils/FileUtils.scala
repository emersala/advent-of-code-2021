package utils

import scala.io.Source


object FileUtils:

    def readListOfStringsFromTxt(path: String): List[String] = 
        Source.fromFile(path)
            .getLines
            .toList

    def readListOfIntsFromTxt(path: String): List[Int] = 
        readListOfStringsFromTxt(path)
            .map((s: String) => s.toInt)