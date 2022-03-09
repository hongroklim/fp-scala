package funsets

/**
 * This class is a test suite for the methods in object FunSets.
 *
 * To run this test suite, start "sbt" then run the "test" command.
 */
class FunSetSuite extends munit.FunSuite {

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
  }

  /**
   * This test is currently disabled (by using @Ignore) because the method
   * "singletonSet" is not yet implemented and the test would fail.
   *
   * Once you finish your implementation of "singletonSet", remove the
   * .ignore annotation.
   */
  test("singleton set one contains one") {

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

  test("intersect contains common elements") {
    new TestSets {
      val u = intersect(union(s1, s2), union(s2, s3))
      assert(!contains(u, 1), "1")
      assert(contains(u, 2), "2")
      assert(!contains(u, 3), "3")
    }
  }

  test("diff contains elements that is S - T") {
    new TestSets {
      val d = diff(union(s1, s2), union(s2, s3))
      assert(contains(d , 1), "1")
      assert(!contains(d, 2), "2")
      assert(!contains(d, 3), "3")
    }
  }

  test("filter returns the subset") {
    new TestSets {
      val u = union(union(s1, s2), s3)
      val f = filter(u, x => (x % 2 != 0))
      assert(contains(f, 1), "1")
      assert(!contains(f, 2), "2")
      assert(contains(f, 3), "3")
    }
  }

  test("forall iters bounded integers") {
    new TestSets {
      val u = union(union(s1, s2), s3)
      assert(forall(u, x => x <= 3), "less or equal to 3")
      assert(!forall(u, x => (x % 2 == 0)), "even numbers")
    }
  }

  test("exists is true if it contains at least one") {
    new TestSets {
      val u = union(union(s1, s2), s3)
      assert(exists(u, x => x <= 3), "less than or equal to 3")
      assert(exists(u, x => (x % 2 == 0)), "even numbers")
      assert(!exists(u, x => x > 3), "greater than3")
    }
  }

  test("map iters all elements and transfroms them") {
    new TestSets {
      val u = union(union(s1, s2), s3)
      val m = map(u, x => x * 10)
      assert(contains(m, 10), 10)
      assert(contains(m, 20), 20)
      assert(contains(m, 30), 30)
    }
  }

  import scala.concurrent.duration._
  override val munitTimeout = 10.seconds
}
