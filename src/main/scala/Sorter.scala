trait Sorter {
  def sort(unsortedSeq: Seq[Int]): Seq[Int]
}

object Sorters {
  val seq = Seq(InsertionSorter, MergeSorter)
}

object InsertionSorter extends Sorter {
  override def sort(unsortedSeq: Seq[Int]): Seq[Int] = {
    unsortedSeq.foldLeft(Seq.empty[Int]) { (seq, e) =>
      val index = seq.indexWhere(_ > e)
      if (index == -1) {
        seq :+ e
      } else {
        seq.take(index) :+ e :++ seq.drop(index)
      }
    }
  }
}

object MergeSorter extends Sorter {
  override def sort(unsortedSeq: Seq[Int]): Seq[Int] = {
    def _sort(leftSeq: Seq[Int], rightSeq: Seq[Int]): Seq[Int] = {
      (leftSeq, rightSeq) match {
        case (left, Nil)  => left
        case (Nil, right) => right
        case (leftHead :: leftTail, rightHead :: rightTail) =>
          if (leftHead < rightHead) leftHead +: _sort(leftTail, rightSeq)
          else rightHead +: _sort(leftSeq, rightTail)
      }
    }
    val pivot = unsortedSeq.size / 2
    if (pivot == 0) unsortedSeq
    else {
      val split = unsortedSeq.splitAt(pivot)
      _sort(sort(split._1), sort(split._2))
    }
  }
}
