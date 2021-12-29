package nl.dennisschroer.adventofcode.year2021

import org.junit.jupiter.api.Assertions.assertEquals
import org.junit.jupiter.api.Test

internal class Day21Test {

    @Test
    fun testPart1() {
        assertEquals(739785, Day21().part1(4, 8))
    }

    @Test
    fun testPart2() {
        assertEquals(444356092776315, Day21().part2(4, 8))
    }
}