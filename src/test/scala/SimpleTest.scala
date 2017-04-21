/**
  * This test only fails when imports and testing in itself doesn't work
  */
import Emulator.Main

import utest._

object SimpleTest extends TestSuite {

  // Initialize App
  def tests = TestSuite {
    'SimpleTest {
      assert(true)
    }
  }
}