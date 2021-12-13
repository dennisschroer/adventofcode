package nl.dennisschroer.adventofcode.year2021

import org.junit.jupiter.api.Assertions.assertEquals
import org.junit.jupiter.api.Test

internal class Day13Test {

    @Test
    fun testPart1() {
        val lines = readLines("testDay13")
        assertEquals(17, Day13().part1(lines))
    }

    @Test
    fun testPart2() {
        val lines = readLines("testDay13")
        assertEquals(0, Day13().part2(lines))
    }
}