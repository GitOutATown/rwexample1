package books.mathematical_methods_linguistics.set_theory.ch1_basic_concepts

object SetOperations_lab_1 {

  val nullSet = Set[Int]()                        //> nullSet  : scala.collection.immutable.Set[Int] = Set()
  // universe
  val U = (1 to 25).toSet                         //> U  : scala.collection.immutable.Set[Int] = Set(5, 10, 24, 25, 14, 20, 1, 6, 
                                                  //| 21, 9, 13, 2, 17, 22, 12, 7, 3, 18, 16, 11, 23, 8, 19, 4, 15)
  val s1 = Set(1,2,3,4,5,6,7,8,9,10)              //> s1  : scala.collection.immutable.Set[Int] = Set(5, 10, 1, 6, 9, 2, 7, 3, 8, 
                                                  //| 4)
  val s2 = Set(2,4,6,8,10)                        //> s2  : scala.collection.immutable.Set[Int] = Set(10, 6, 2, 8, 4)
  val s3 = Set(11, 12, 13)                        //> s3  : scala.collection.immutable.Set[Int] = Set(11, 12, 13)
  
  s1.union(s2)                                    //> res0: scala.collection.immutable.Set[Int] = Set(5, 10, 1, 6, 9, 2, 7, 3, 8, 
                                                  //| 4)
  s1.intersect(s2)                                //> res1: scala.collection.immutable.Set[Int] = Set(10, 6, 2, 8, 4)
  
  s2 -- s1                                        //> res2: scala.collection.immutable.Set[Int] = Set()
  
  // I'm not seeing any difference between -- and diff
  (U -- s1).toList.sorted                         //> res3: List[Int] = List(11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 2
                                                  //| 4, 25)
  U.diff(s1).toList.sorted                        //> res4: List[Int] = List(11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 2
                                                  //| 4, 25)
  Set(1,2,3) -- Set(0,1,2,3,4)                    //> res5: scala.collection.immutable.Set[Int] = Set()
  Set(1,2,3) -- Set(11,12)                        //> res6: scala.collection.immutable.Set[Int] = Set(1, 2, 3)
  
  // pseudo compliment, curried for partial function set up
  def compliment(universe: Set[Int])(s: Set[Int]): Set[Int] = {
    universe -- s
  }                                               //> compliment: (universe: Set[Int])(s: Set[Int])Set[Int]
  def isSubset(container: Set[Int])(sub: Set[Int]): Boolean = {
    (sub union container) == container &&
    (sub intersect container) == sub
  }                                               //> isSubset: (container: Set[Int])(sub: Set[Int])Boolean
  
  val comp = compliment(U)_                       //> comp  : Set[Int] => Set[Int] = <function1>
  
  comp(s1).toList.sorted                          //> res7: List[Int] = List(11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 2
                                                  //| 4, 25)
  comp(s2).toList.sorted                          //> res8: List[Int] = List(1, 3, 5, 7, 9, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20
                                                  //| , 21, 22, 23, 24, 25)
  
  /** Some fundamental set-theoretic equalities **/
  // TODO: For these examples to be more meaningful, I should set them up as scalacheck tests
  
  // 1. Idempotent Laws
  // a. X union X = X
  (s1 union s1) == s1                             //> res9: Boolean = true
  // b. X intersect X = X
  (s1 intersect s1) == s1                         //> res10: Boolean = true
  
  // 2. Commutative Laws
  // a. X union Y = Y union X
  (s1 union s2) == (s2 union s1)                  //> res11: Boolean = true
  // b. X intersect Y = Y intersect X
  (s1 intersect s2) == (s2 intersect s1)          //> res12: Boolean = true
  
  // 3. Associative Laws
  // a. (X union Y) union Z = X union (Y union Z)
  ((s1 union s2) union s3) == (s1 union (s2 union s3))
                                                  //> res13: Boolean = true
  // b. (X intersect Y) intersect Z = X intersect (Y intersect Z)
  ((s1 intersect s2) intersect s3) == (s1 intersect (s2 intersect s3))
                                                  //> res14: Boolean = true
  // 4. Distributive Laws
  // a. X union (Y intersect Z) = (X union Y) intersect (X union Z)
  (s1 union (s2 intersect s3)) == ((s1 union s2) intersect (s1 union s3))
                                                  //> res15: Boolean = true
  // b. X intersect (Y union Z) = (X intersect Y) union (X intersect Z)
  (s1 intersect (s2 union s3)) == ((s1 intersect s2) union (s1 intersect s3))
                                                  //> res16: Boolean = true
  // 5. Identity Laws
  // a. X union null set = X
  (s1 union nullSet) == s1                        //> res17: Boolean = true
  // b. X union univers = univers
  (s1 union U) == U                               //> res18: Boolean = true
  // c. X intersect nullSet = nullSet
  (s1 intersect nullSet) == nullSet               //> res19: Boolean = true
  // d. X intersect U = X
  (s1 intersect U) == s1                          //> res20: Boolean = true
  
  // 6. Complement Laws
  // a. X union X' = U
  (s1 union comp(s1)) == U                        //> res21: Boolean = true
  // b. (X')' = X
  (comp(comp(s1))) == s1                          //> res22: Boolean = true
  // c. X intersect X' = nullSet
  (s1 intersect comp(s1)) == nullSet              //> res23: Boolean = true
  // d. X - Y = X intersect Y'
  (s1 -- s2) == (s1 intersect comp(s2))           //> res24: Boolean = true
  
  // 7. DeMorgan's Law
  // a. (X union Y)' = X' intersect Y'
  (comp(s1 union s2)) == (comp(s1) intersect comp(s2))
                                                  //> res25: Boolean = true
  // b. (X intersect Y)' = X' union Y'
  (comp(s1 intersect s2)) == (comp(s1) union comp(s2))
                                                  //> res26: Boolean = true
  // 8. Consistency Principal
  // a. X isASubsetOf Y iff X union Y = Y
  s2.subsetOf(s1) && (s2 union s1) == s1          //> res27: Boolean = true
  // b. X isASubsetOf Y iff X intersect Y = X
  s2.subsetOf(s1) && (s2 intersect s1) == s2      //> res28: Boolean = true
  isSubset(s1)(s2) // s2 isSubset s1              //> res29: Boolean = true
  //isSubset(s2)(s1) // false
  '''                                             //> res30: Char('\'') = '
}
/*




*/