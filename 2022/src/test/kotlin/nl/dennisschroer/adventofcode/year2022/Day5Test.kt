package nl.dennisschroer.adventofcode.year2022

import org.junit.jupiter.api.Assertions.assertEquals
import org.junit.jupiter.api.Test

internal class Day5Test {

    @Test
    fun testPart1() {
        val lines = readBlocks("day5")
        assertEquals("CMZ", day5part1(lines))
    }

    @Test
    fun testPart2() {
        val lines = readBlocks("day5")
        assertEquals(0, day5part2(lines))
    }
}