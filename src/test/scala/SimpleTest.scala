/**
  * This test only fails when imports and testing in itself doesn't work
  */
import Emulator.Main

import utest._

import org.scalajs.jquery.jQuery

object SimpleTest extends TestSuite {

  // Initialize App
  def tests = TestSuite {
    'UITests {
      assert(jQuery("p:contains('Something that does not exists')").length == 0)
    }
  }
}