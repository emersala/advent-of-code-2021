package day.one
import SubmarineDepth.*

trait SubmarineDepthSuite  extends  munit.FunSuite:
    import SubmarineDepth.*

    trait TestDepths :
        var depthList : List[Int] = List(199, 200, 208, 210, 200, 207, 240, 269, 260, 263)


    test("times depth increased") {
        new TestDepths:
            assertEquals(countDeepening(depthList), 7)
    }

    test("sliding windows depth increased") {
        new TestDepths :
            assertEquals(countSliding(depthList), 5)
    }