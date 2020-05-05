import org.scalatest.funsuite.AnyFunSuite

class SorterSpec extends AnyFunSuite {
  test("all sort") {
    val unsortedSeq = Seq(3, 6, 2, 7, 1, 8, 5, 4)
    val sortedSeq = Seq(1, 2, 3, 4, 5, 6, 7, 8)
    Sorters.seq.foreach { sorter =>
      println(s"sorter: ${sorter.getClass}")
      assertResult(sortedSeq)(sorter.sort(unsortedSeq))
    }
  }
}
