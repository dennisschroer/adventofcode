package nl.dennisschroer.adventofcode.year2021

import org.junit.jupiter.api.Assertions.assertEquals
import org.junit.jupiter.api.Test

internal class Day6Test {

    @Test
    fun testPart1() {
        val lines = readLines("testDay6")
        assertEquals(5934, Day6().part1(lines[0]))
    }

    @Test
    fun testPart2() {
        val lines = readLines("testDay6")
        assertEquals(26984457539, Day6().part2(lines[0]))
    }
}