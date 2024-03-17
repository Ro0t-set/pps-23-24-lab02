
package task02


import org.junit.Test
import org.junit.Assert._

class MainTest {

  // 3a) Tests
  @Test def testGetTheSignMethodStyle(): Unit = {
    assertEquals("positive", Main.getTheSignMethodStyle(5))
    assertEquals("negative", Main.getTheSignMethodStyle(-5))
  }

  @Test def testGetTheSignLambdaStyle(): Unit = {
    assertEquals("positive", Main.getTheSignLambdaStyle(5))
    assertEquals("negative", Main.getTheSignLambdaStyle(-5))
  }

  // 3b) Tests
  @Test def testNegMethodStyle(): Unit = {
    val emptyString: String => Boolean = _ == ""
    val notEmpty = Main.negMethodStyle(emptyString)
    assertTrue(notEmpty("foo"))
    assertFalse(notEmpty(""))
  }

  @Test def testNegLambdaStyle(): Unit = {
    val emptyString: String => Boolean = _ == ""
    val notEmpty = Main.negLambdaStyle(emptyString)
    assertTrue(notEmpty("foo"))
    assertFalse(notEmpty(""))
  }

  // 3c) Tests
  @Test def testNegMethodStyleGeneric(): Unit = {
    val isEven: Int => Boolean = _ % 2 == 0
    val isOdd = Main.negMethodStyleGeneric(isEven)
    assertTrue(isOdd(3))
    assertFalse(isOdd(4))
  }

  // 4) Tests
  @Test def testXyzStandard(): Unit = {
    assertTrue(Main.xyzStandard(1, 1, 1))
    assertFalse(Main.xyzStandard(1, 2, 3))
  }

  @Test def testXyzCurryed(): Unit = {
    assertTrue(Main.xyzCurryed(1)(1)(1))
    assertFalse(Main.xyzCurryed(1)(2)(3))
  }

  @Test def testXyzLambdaStandard(): Unit = {
    assertTrue(Main.xyzLambdaStandard(1, 1, 1))
    assertFalse(Main.xyzLambdaStandard(1, 2, 3))
  }

  @Test def testXyzLambdaCurryed(): Unit = {
    assertTrue(Main.xyzLambdaCurryed(1)(1)(1))
    assertFalse(Main.xyzLambdaCurryed(1)(2)(3))
  }

  // 5) Tests
  @Test def testCompose(): Unit = {
    val f: Int => Int = _ - 1
    val g: Int => Int = _ * 2
    val composed = Main.compose(f, g)
    assertEquals(9, composed(5))
  }

  @Test def testGenericCompose(): Unit = {
    val f: String => Int = _.length
    val g: Int => String = _.toString
    val composed = Main.genericCompose(f, g)
    assertEquals(5, composed(12345))
  }

  // 6) Tests
  @Test def testGcd(): Unit = {
    assertEquals(4, Main.gcd(12, 8))
    assertEquals(7, Main.gcd(14, 7))
  }

  // 7) Tests
  @Test def testShapePerimeter(): Unit = {
    val rectangle = Main.Shape.Rectangle(2, 3)
    val circle = Main.Shape.Circle(5)
    val square = Main.Shape.Square(4)

    assertEquals(10, Main.Shape.perimeter(rectangle), 0.01)
    assertEquals(31.42, Main.Shape.perimeter(circle), 0.01)
    assertEquals(16, Main.Shape.perimeter(square), 0.01)
  }

  @Test def testShapeScale(): Unit = {
    val rectangle = Main.Shape.Rectangle(2, 3)
    val circle = Main.Shape.Circle(5)
    val square = Main.Shape.Square(4)

    val scaledRectangle = Main.Shape.scale(rectangle, 2)
    val scaledCircle = Main.Shape.scale(circle, 0.5)
    val scaledSquare = Main.Shape.scale(square, 1.5)

    assertEquals(4, scaledRectangle.asInstanceOf[Main.Shape.Rectangle].w, 0.01)
    assertEquals(6, scaledRectangle.asInstanceOf[Main.Shape.Rectangle].h, 0.01)

    assertEquals(2.5, scaledCircle.asInstanceOf[Main.Shape.Circle].r, 0.01)

    assertEquals(6, scaledSquare.asInstanceOf[Main.Shape.Square].l, 0.01)
  }

  // 8) Tests
  @Test def testOptionalIsEmpty(): Unit = {
    val maybeValue: Main.Optional[Int] = Main.Optional.Maybe(5)
    val emptyValue: Main.Optional[Int] = Main.Optional.Empty()

    assertFalse(Main.Optional.isEmpty(maybeValue))
    assertTrue(Main.Optional.isEmpty(emptyValue))
  }

  @Test def testOptionalOrElse(): Unit = {
    val maybeValue: Main.Optional[Int] = Main.Optional.Maybe(5)
    val emptyValue: Main.Optional[Int] = Main.Optional.Empty()

    assertEquals(5, Main.Optional.orElse(maybeValue, 10))
    assertEquals(10, Main.Optional.orElse(emptyValue, 10))
  }

  @Test def testOptionalMap(): Unit = {
    val maybeValue: Main.Optional[Int] = Main.Optional.Maybe(5)
    val emptyValue: Main.Optional[Int] = Main.Optional.Empty()

    val mappedMaybe = Main.Optional.map(maybeValue)(_ > 2)
    val mappedEmpty = Main.Optional.map(emptyValue)(_ > 2)

    assertTrue(Main.Optional.isEmpty(mappedEmpty))
    assertTrue(mappedMaybe.asInstanceOf[Main.Optional.Maybe[Boolean]].value)
  }

  @Test def testOptionalFilter(): Unit = {
    val maybeValue: Main.Optional[Int] = Main.Optional.Maybe(5)
    val emptyValue: Main.Optional[Int] = Main.Optional.Empty()

    val filteredMaybe1 = Main.Optional.filter(maybeValue)(_ > 2)
    val filteredMaybe2 = Main.Optional.filter(maybeValue)(_ > 8)
    val filteredEmpty = Main.Optional.filter(emptyValue)(_ > 2)

    assertTrue(filteredMaybe1.asInstanceOf[Main.Optional.Maybe[Int]].value == 5)
    assertTrue(Main.Optional.isEmpty(filteredMaybe2))
    assertTrue(Main.Optional.isEmpty(filteredEmpty))
  }
}