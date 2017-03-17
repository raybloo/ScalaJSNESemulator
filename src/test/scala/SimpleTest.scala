/**
  * Created by trolo on 27.02.2017.
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