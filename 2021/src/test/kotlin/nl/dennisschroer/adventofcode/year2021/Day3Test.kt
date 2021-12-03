package nl.dennisschroer.adventofcode.year2021

import org.junit.jupiter.api.Assertions.assertEquals
import org.junit.jupiter.api.Test

internal class Day3Test {

    @Test
    fun testPart1() {
        val lines = readLines("testDay3")

        assertEquals("10110", Day3().gammaRate(lines))
        assertEquals("01001", Day3().epsilonRate(lines))
        assertEquals(198, Day3().part1(lines))
    }

    @Test
    fun testPart2() {
        val lines = readLines("testDay3")

        assertEquals("10111", Day3().oxygenGeneratorRating(lines))
        assertEquals("01010", Day3().co2ScrubberRating(lines))
        assertEquals(230, Day3().part2(lines))
    }
}