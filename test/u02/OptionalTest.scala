package u02

import org.junit.jupiter.api.Assertions.assertEquals
import org.junit.jupiter.api.Test
import u02.Optionals.Option.*
import ex.functions.{map, filter, fold}

class OptionalTest:

  @Test def filterTest(): Unit =
    assertEquals(filter(Some(5))(_ > 2), Some(5))
    assertEquals(filter(Some(5))(_ > 8), None())
    assertEquals(filter(None[Int]())(_ > 2), None())

  @Test def mapTest(): Unit =
    assertEquals(map(Some(5))(_ > 2), Some(true))
    assertEquals(map(Some(5))(_ > 8), Some(false))
    assertEquals(map(None[Int]())(_ > 2), None())

  @Test def foldTest(): Unit =
    assertEquals(fold(Some(5))(1)(_ + 1), 6)
    assertEquals(fold(None[Int]())(1)(_ + 1), 1)
