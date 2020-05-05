trait Main {

  // insertion sort
  def sort(unsortedSeq: Seq[Int]): Seq[Int] = {
    unsortedSeq.foldLeft(Seq.empty[Int]) { (seq, e) =>
      val index = seq.indexWhere(_ > e)
      if (index == -1) {
        seq :+ e
      } else {
        (seq.take(index) :+ e) ++ seq.drop(index)
      }
    }
  }

}