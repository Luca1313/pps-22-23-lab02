package u02

import org.junit.jupiter.api.Assertions.*
import org.junit.jupiter.api.Test
import ex.functions.gcd

class GcdTest:

  @Test def gcdTest(): Unit =
    assertEquals(gcd(144, 12), 12)
    assertEquals(gcd(19, 29), 1)
