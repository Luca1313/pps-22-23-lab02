package u02

import org.junit.jupiter.api.Assertions.*
import org.junit.jupiter.api.Test
import ex.functions.Shape.*
import ex.functions.contains

class ContainsTest:

  @Test def squareTest(): Unit =
    assertTrue(contains(Square((0, 0), 5), (4, 4)))
    assertTrue(contains(Square((4, 4), 5), (8, 5)))
    assertFalse(contains(Square((4, 5), 3), (2, 6)))

  @Test def rectangleTest(): Unit =
    assertTrue(contains(Rectangle((0, 0), 5, 4), (4, 3)))
    assertTrue(contains(Rectangle((3, 3), 2, 4), (4, 6)))
    assertFalse(contains(Rectangle((5, 6), 1, 2), (5, 6)))

  @Test def circleTest(): Unit =
    assertTrue(contains(Circle((0, 0), 3), (2, 2)))
    assertTrue(contains(Circle((2, 2), 4), (-1, 0)))
    assertFalse(contains(Circle((2, 2), 4), (-4, 0)))
