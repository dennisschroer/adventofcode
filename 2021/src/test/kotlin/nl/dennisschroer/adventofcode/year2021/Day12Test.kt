package nl.dennisschroer.adventofcode.year2021

import org.junit.jupiter.api.Assertions.assertEquals
import org.junit.jupiter.api.Test

internal class Day12Test {

    @Test
    fun testPart1() {
        val lines = readLines("testDay12")
        assertEquals(10, Day12().part1(lines))
    }

    @Test
    fun testPart2() {
        val lines = readLines("testDay12")
        assertEquals(0, Day12().part2(lines))
    }
}