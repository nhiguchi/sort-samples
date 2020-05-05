import org.scalatest.funsuite.AnyFunSuite

class SorterSpec extends AnyFunSuite {

  val UnsortedSeq = Seq(3, 6, 2, 7, 9, 1, 8, 5, 4)
  val SortedSeq = Seq(1, 2, 3, 4, 5, 6, 7, 8, 9)

  test("insertion sort") {
    assertResult(SortedSeq)(InsertionSorter.sort(UnsortedSeq))
  }

}
