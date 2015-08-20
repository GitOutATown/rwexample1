package lab.bigO

object Big_O_notation_lab_1 {

  // Content source:
  // https://rob-bell.net/2009/06/a-beginners-guide-to-big-o-notation/
  /** Big O specifically describes the worst-case scenario, and can be
   *  used to describe the execution time required or the space used
   *  (e.g. in memory or on disk) by an algorithm.
   */

  /* O(1)
   * describes an algorithm that will always execute in the same
   * time (or space) regardless of the size of the input data set.
   */
  def isFirstElemNull[T](list: List[T]): Boolean = {
    list(0) == null
  }                                               //> isFirstElemNull: [T](list: List[T])Boolean
  
  /* O(N)
   * describes an algorithm whose performance will grow linearly
   * and in direct proportion to the size of the input data set.
   * Big O notation will always assume the upper limit where the
   * algorithm will perform the maximum number of iterations.
   * Here, N = list.length
   */
  def containsValue[T](list: List[T], item: T): Boolean = {
    list match {
      case Nil => false
      case x :: xs => if (x == item) true else containsValue(xs, item)
    }
  }                                               //> containsValue: [T](list: List[T], item: T)Boolean
  
  /* O(N^2)
   * represents an algorithm whose performance is directly
   * proportional to the square of the size of the input data set.
   * This is common with algorithms that involve nested iterations
   * over the data set. Deeper nested iterations will result in
   * O(N^3), O(N^4) etc.
   * RW: Breaking out of a loop is a bit tricky in Scala. Such an
   * attempt at optimization isn't necessary for purposes of explicating
   * O(N^2), but I just added it in for academic inquiry.
   */
  def containsDuplicates[T](list: List[T]): Boolean = {
    val res = (for{
      i <- 0 to list.length-1
      j <- 0 to list.length-1
      if (i != j && list(i) == list(j))
    } yield true).headOption
    res.contains(true)
  }                                               //> containsDuplicates: [T](list: List[T])Boolean
  containsDuplicates(List(1,2,3))                 //> res0: Boolean = false
  containsDuplicates(List(1,2,3,1))               //> res1: Boolean = true
  
  /* O(2N)
   * denotes an algorithm whose growth will double with each additional
   * element in the input data set. The execution time of an O(2N)
   * function will quickly become very large.
   */
   
   /* O(log N)
    * Binary search is a technique used to search sorted data sets. It works by selecting the middle element of the data set, essentially the median, and compares it against a target value. If the values match it will return success. If the target value is higher than the value of the probe element it will take the upper half of the data set and perform the same operation against it. Likewise, if the target value is lower than the value of the probe element it will perform the operation against the lower half. It will continue to halve the data set with each iteration until the value has been found or until it can no longer split the data set.
    * This type of algorithm is described as O(log N). The iterative halving of data sets described in the binary search example produces a growth curve that peaks at the beginning and slowly flattens out as the size of the data sets increase e.g. an input data set containing 10 items takes one second to complete, a data set containing 100 items takes two seconds, and a data set containing 1000 items will take three seconds. Doubling the size of the input data set has little effect on its growth as after a single iteration of the algorithm the data set will be halved and therefore on a par with an input data set half the size. This makes algorithms like binary search extremely efficient when dealing with large data sets.
    */
  
}