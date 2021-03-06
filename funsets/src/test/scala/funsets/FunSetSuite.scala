package funsets

import org.scalatest.FunSuite


import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

/**
 * This class is a test suite for the methods in object FunSets. To run
 * the test suite, you can either:
 *  - run the "test" command in the SBT console
 *  - right-click the file in eclipse and chose "Run As" - "JUnit Test"
 */
@RunWith(classOf[JUnitRunner])
class FunSetSuite extends FunSuite {

  /**
   * Link to the scaladoc - very clear and detailed tutorial of FunSuite
   *
   * http://doc.scalatest.org/1.9.1/index.html#org.scalatest.FunSuite
   *
   * Operators
   *  - test
   *  - ignore
   *  - pending
   */

  /**
   * Tests are written using the "test" operator and the "assert" method.
   */
  // test("string take") {
  //   val message = "hello, world"
  //   assert(message.take(5) == "hello")
  // }

  /**
   * For ScalaTest tests, there exists a special equality operator "===" that
   * can be used inside "assert". If the assertion fails, the two values will
   * be printed in the error message. Otherwise, when using "==", the test
   * error message will only say "assertion failed", without showing the values.
   *
   * Try it out! Change the values so that the assertion fails, and look at the
   * error message.
   */
  // test("adding ints") {
  //   assert(1 + 2 === 3)
  // }


  import FunSets._

  test("contains is implemented") {
    assert(contains(x => true, 100))
  }

  /**
   * When writing tests, one would often like to re-use certain values for multiple
   * tests. For instance, we would like to create an Int-set and have multiple test
   * about it.
   *
   * Instead of copy-pasting the code for creating the set into every test, we can
   * store it in the test class using a val:
   *
   *   val s1 = singletonSet(1)
   *
   * However, what happens if the method "singletonSet" has a bug and crashes? Then
   * the test methods are not even executed, because creating an instance of the
   * test class fails!
   *
   * Therefore, we put the shared values into a separate trait (traits are like
   * abstract classes), and create an instance inside each test method.
   *
   */

  trait TestSets {
    val s1 = singletonSet(1)
    val s2 = singletonSet(2)
    val s3 = singletonSet(3)
    val s4 = { (x: Int) => x < 3 }
  }

  /**
   * This test is currently disabled (by using "ignore") because the method
   * "singletonSet" is not yet implemented and the test would fail.
   *
   * Once you finish your implementation of "singletonSet", exchange the
   * function "ignore" by "test".
   */
  test("singletonSet(1) contains 1") {

    /**
     * We create a new instance of the "TestSets" trait, this gives us access
     * to the values "s1" to "s3".
     */
    new TestSets {
      /**
       * The string argument of "assert" is a message that is printed in case
       * the test fails. This helps identifying which assertion failed.
       */
      assert(contains(s1, 1), "Singleton")
    }
  }

  test("union contains all elements of each set") {
    new TestSets {
      val s = union(s1, s2)
      assert(contains(s, 1), "Union 1")
      assert(contains(s, 2), "Union 2")
      assert(!contains(s, 3), "Union 3")
    }
  }

  test("intersect returns the intersection of the two given sets") {
    new TestSets {
      val set1 = intersect(s1, s4)
      assert(contains(set1, 1), "Intersect 1")
      assert(!contains(set1, 2), "Intersect 2")
      assert(!contains(set1, 3), "Intersect 3")

      val set2 = intersect(s3, s4)
      assert(!contains(set2, 3), "Intersect 3")
    }
  }

  test("diff returns the difference of the two given sets") {
    new TestSets {
      val s = diff(s4, s2)
      assert(contains(s, 1), "Diff 1")
      assert(contains(s, -1), "Diff -1")
      assert(!contains(s, 2), "Diff 2")
    }
  }

  test("filter returns the subset of `s` for which `p` holds") {
    new TestSets {
      val set1 = filter(s1, {(x: Int) => x > 2})
      assert(!contains(set1, 1), "Filter 1")
      assert(!contains(set1, 2), "Filter 2")
      assert(!contains(set1, 3), "Filter 3")

      val set2 = filter(s3, {(x: Int) => x > 2})
      assert(!contains(set2, 1), "Filter 1")
      assert(!contains(set2, 2), "Filter 2")
      assert(contains(set2, 3), "Filter 3")
    }
  }

  test("forall returns whether all bounded integers within `s` satisfy `p`") {
    val set1 = {(x: Int) => x > 2}
    val set2 = singletonSet(2)
    val p = {(y: Int) => (y%2 == 0)}

    val bool_answer1 = forall(set2, p)
    assert(bool_answer1)

    val bool_answer2 = forall(set1, p)
    assert(!bool_answer2)
  }

  test("exists returns whether there exists a bounded integer within `s` that satisfies `p`") {
    val set1 = {(x: Int) => x > 2}
    val set2 = singletonSet(3)
    val p = {(y: Int) => (y%2 == 0)}
    val p2 = {(y: Int) => (y * 2 == 6)}

    val bool_answer1 = exists(set2, p)
    assert(!bool_answer1)

    val bool_answer2 = exists(set1, p)
    assert(bool_answer2)

    val bool_answer3 = exists(set2, p2)
    assert(bool_answer3)
  }

  test("map returns a set transformed by applying `f` to each element of `s`") {
    val set2 = {(x: Int) => x > 2}
    val set1 = singletonSet(3)
    val p = {(y: Int) => y * 2}

    val answer_set1 = map(set1, p)
    assert(contains(answer_set1, 6), "Contains 6")
    assert(!contains(answer_set1, 3), "Does not contain 3")

    val answer_set2 = map(set2, p)
    assert(contains(answer_set2, 6), "Contains 6")
    assert(contains(answer_set2, 8), "Contain 8")
    assert(!contains(answer_set2, 0), "Does not contain 0")
  }
}
