package nl.dennisschroer.adventofcode.year2021

import org.junit.jupiter.api.Assertions.assertEquals
import org.junit.jupiter.api.Test

internal class Day18Test {

    @Test
    fun testPart1() {
        val lines = readLines("testDay18")
        assertEquals(4140, Day18().part1(lines))
    }

    @Test
    fun testPart2() {
        val lines = readLines("testDay18")
        assertEquals(0, Day18().part2(lines))
    }
}