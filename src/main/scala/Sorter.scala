trait Sorter {
  def sort(seq: Seq[Int]): Seq[Int]
}

object Sorters {
  val seq = Seq(InsertionSorter, MergeSorter, QuickSorter)
}

object InsertionSorter extends Sorter {
  override def sort(seq: Seq[Int]): Seq[Int] = {
    seq.foldLeft(Seq.empty[Int]) { (seq, e) =>
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
  override def sort(seq: Seq[Int]): Seq[Int] = {
    def _sort(leftSeq: Seq[Int], rightSeq: Seq[Int]): Seq[Int] = {
      (leftSeq, rightSeq) match {
        case (left, Nil)  => left
        case (Nil, right) => right
        case (leftHead :: leftTail, rightHead :: rightTail) =>
          if (leftHead < rightHead) leftHead +: _sort(leftTail, rightSeq)
          else rightHead +: _sort(leftSeq, rightTail)
      }
    }
    val pivot = seq.size / 2
    if (pivot == 0) seq
    else {
      val split = seq.splitAt(pivot)
      _sort(sort(split._1), sort(split._2))
    }
  }
}

object QuickSorter extends Sorter {
  override def sort(seq: Seq[Int]): Seq[Int] = {
    if (seq.size <= 1) seq
    else {
      val pivot = seq(seq.size / 2)
      sort(seq.filter(_ < pivot)) ++
        seq.filter(_ == pivot) ++
        sort(seq.filter(pivot < _))
    }
  }
}
