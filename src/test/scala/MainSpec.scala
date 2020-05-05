import org.scalatest.funsuite.AnyFunSuite

class MainSpec extends AnyFunSuite with Main {

  test("insertion sort") {
    // given
    val unsortedSeq = Seq(3, 6, 2, 7, 9, 1, 8, 5, 4)
    assertResult(Seq(1, 2, 3, 4, 5, 6, 7, 8, 9))(sort(unsortedSeq))
  }

}
