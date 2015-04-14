package quickcheck

import common._
import scala.annotation.tailrec

import org.scalacheck._
//Relative imports from scalacheck, otherwise would be org.scalacheck.Arbitrary._
import Arbitrary._
import Gen._
import Prop._ //if we remove this we have to use Prop.forall instead of forall

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {
  //SCALACHECK HELP : https://github.com/rickynils/scalacheck/wiki/User-Guide#generators

  //we need to create the generator before using it
  val ints = Gen.choose(0, 100) 
  //Generator container that is a List of Ints using the generator ints for the list values
  val genIntList = Gen.containerOf[List,Int](ints)
  
  //this is to group properties we identify it with ("min_of_1")
  //property("min_of_1") = forAll { a: A =>
  property("min_of_1") = forAll(ints) { a =>
    val h = insert(a, empty)
    findMin(h) == a
  }

  property("min_of_2_ordered") = forAll(ints,ints) { (a,b) =>

    val h = insert(a, empty)
    val h2 = insert(b,h) 
    findMin(h2) == ord.min(a, b)
  }

  property("min_insert") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }
   
  property("delete_from_one_elem") = forAll(ints) { a =>
    val h = insert(a, empty)
    //this a label to be able to identify the fail easily, is more useful
    //when the property has several checks
    "the one and only check delete has failed" |: deleteMin(h) == empty
  }

  property("insert_list_ascending_ints") = forAll(genIntList) { intList =>

    val sortedIntList = intList.sorted

    //we could have done this with var an a for loop
    def buildHeap(accHeap: H, accList: List[Int]): H = {
      if (accList.isEmpty) accHeap
      else buildHeap(insert(accList.head, accHeap), accList.tail)
    }

    //we check that the findmin in every iteration is the same as the ascending list element
    def checkHeapMin(accHeap: H, accList: List[Int]): Boolean = {
      if (accList.isEmpty) true
      else {
        if (findMin(accHeap) != accList.head) false
        else checkHeapMin(deleteMin(accHeap), accList.tail)
      }
    }
    checkHeapMin(buildHeap(empty, sortedIntList), sortedIntList)
  }
  
  //the arbitrary generator of H is implicit lazy val arbHeap so it will be used 
  //automatically to generate the H instances
  property("minimum_of_meld") = forAll { (h1: H, h2: H) =>
     val h = meld(h1, h2)
    //combined check, we also have the method all to check all the conditions
    atLeastOne(findMin(h) == findMin(h1), findMin(h) == findMin(h2))
    //dirty way
    //(findMin(h) == findMin(h1)) || (findMin(h) ==findMin(h1))		  
  }
  
  property("ordered_creating_a_new_heap") = forAll { (h: H) =>
    if(isEmpty(h)) true
    else heapHelper(deleteMin(h),findMin(h))   
  }
  @tailrec //IMP : we need to do it final in order to apply tailrec
  final def heapHelper(accHeap: H, elem: A): Boolean = {
    //we delete all the elements from one list and we include it on the other, in the end they must have the same order
    if (isEmpty(accHeap)) true
    else {
      if (!ord.lteq(elem, findMin(accHeap))) true
      else heapHelper(deleteMin(accHeap), findMin(accHeap))
    }
  }
 
  //custom type generator IMP this is not generating empty heaps
  lazy val genHeap: Gen[H] = for {
    //we get an int value between 0 and 100
    x <- Gen.choose(0, 100)
    //we choose either a new empty list or the recursive value of genHeap
    h <- oneOf[H](empty, genHeap)
  } yield insert(x, h)

  // implicit arbitrary using the generator of H , this will be used in the properties [H]
  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  //--------------------------------------------
  // Map generator example:
  //  lazy val genMap: Gen[Map[Int,Int]] = for {
  //  k <- arbitrary[Int]
  //  v <- arbitrary[Int]
  //  m <- oneOf(value(Map.empty[Int,Int]), genMap)
  //} yield m.updated(k, v)

  //Tree generator exmple
  // val genLeaf = value(Leaf)
  //
  // val genNode = for {
  //   v <- arbitrary[Int]
  //   left <- genTree
  //   right <- genTree
  // } yield Node(left, right, v)
  //
  // def genTree: Gen[Tree] = oneOf(genLeaf, genNode)

}
