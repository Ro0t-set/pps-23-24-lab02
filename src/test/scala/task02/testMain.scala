
package task02

import org.junit.Test
import org.junit.Assert._

class MainTest:
  @Test def testGetTheSignMethodStylePositive(): Unit = {
    val result = Main.getTheSignMethodStyle(5)
    assertEquals("positive", result)
  }

  @Test def testGetTheSignMethodStyleNegative(): Unit = {
    val result = Main.getTheSignMethodStyle(-5)
    assertEquals("negative", result)
  }

  @Test def testGetTheSignMethodStyleZero(): Unit = {
    val result = Main.getTheSignMethodStyle(0)
    assertEquals("positive", result)
  }

  @Test def testGetTheSignLambdaStylePositive(): Unit = {
    val result = Main.getTheSignLambdaStyle(5)
    assertEquals("positive", result)
  }

  @Test def testGetTheSignLambdaStyleNegative(): Unit = {
    val result = Main.getTheSignLambdaStyle(-5)
    assertEquals("negative", result)
  }

  @Test def testGetTheSignLambdaStyleZero(): Unit = {
    val result = Main.getTheSignLambdaStyle(0)
    assertEquals("positive", result)
  }