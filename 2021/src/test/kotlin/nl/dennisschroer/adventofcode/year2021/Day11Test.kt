package nl.dennisschroer.adventofcode.year2021

import org.junit.jupiter.api.Assertions.assertEquals
import org.junit.jupiter.api.Test

internal class Day11Test {

    @Test
    fun testPart1() {
        val lines = readLines("testDay11")
        assertEquals(1656, Day11().part1(lines))
    }

    @Test
    fun testPart2() {
        val lines = readLines("testDay11")
        assertEquals(195, Day11().part2(lines))
    }
}