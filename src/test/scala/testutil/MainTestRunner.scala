package testutil

import insynth.reconstruction._
import insynth.reconstruction.intermediate._
import insynth.util.streams.ordered._

import lesynth.TryOutTest

import org.junit.Test
import org.junit.Ignore

// useful for running the test directly (avoiding the test framework)
object MainTestRunner {

  def main(args: Array[String]): Unit =
    (new TryOutTest).test1
    
}

class MainTestRunner {

  @Test
  def main: Unit =
    Unit
    
}