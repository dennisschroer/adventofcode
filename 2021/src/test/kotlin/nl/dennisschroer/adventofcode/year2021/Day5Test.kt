package nl.dennisschroer.adventofcode.year2021

import org.junit.jupiter.api.Assertions.assertEquals
import org.junit.jupiter.api.Test

internal class Day5Test {

    @Test
    fun testPart1() {
        val lines = readLines("testDay5")
        assertEquals(5, Day5().part1(lines))
    }

    @Test
    fun testPart2() {
        val lines = readLines("testDay5")
        assertEquals(12, Day5().part2(lines))
    }
}