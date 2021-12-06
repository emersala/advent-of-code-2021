package day.three

trait BinaryDiagnoticSuite extends munit.FunSuite:
    import utils.FileUtils     

     trait TestDepths :
        var binaryReport : List[Array[Int]] = FileUtils.readListOfStringsFromTxt("src/test/scala/day/two/directions.txt")
                                                .map(s => s.toCharArray.map(c => c.toInt))

     
     test {
         assertEquals(getDiagnostic(binaryReport).gamma, 22)
     }
     
     test {
         assertEquals(getDiagnostic(binaryReport).epsilton, 9)
     }