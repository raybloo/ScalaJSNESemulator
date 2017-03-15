/**
  * Created by trolo on 27.02.2017.
  */
import tutorial.webapp.HelloWorldApp

import utest._

import org.scalajs.jquery.jQuery

object HelloWorldTest extends TestSuite {

  // Initialize App
  HelloWorldApp.setupUI()

  def tests = TestSuite {
    'HelloWorld {
      assert(jQuery("p:contains('Hello World')").length == 1)
    }
  }
}