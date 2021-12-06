package day.two

trait SubmarinePositionSuite  extends  munit.FunSuite:
    import SubmarinePositionWithoutAim.*
    import SubmarinePositionWithAim.*
    import utils.FileUtils

    trait TestDepths :
        var depthList : List[String] = FileUtils.readListOfStringsFromTxt("src/test/scala/day/two/directions.txt")

    test("submarine position") {
        new TestDepths:
            assertEquals(SubmarinePositionWithoutAim.getPosition(depthList), 150)
    }

    test("submarine position using aim (part 2)") {
        new TestDepths:
            assertEquals(SubmarinePositionWithAim.getPosition(depthList), 900)
    }