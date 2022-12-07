package nl.dennisschroer.adventofcode.year2022

import org.junit.jupiter.api.Assertions.assertEquals
import org.junit.jupiter.api.Test

internal class Day3Test {

    @Test
    fun testPart1() {
        val lines = readLines("day3")
        assertEquals(157, day3part1(lines))
    }

    @Test
    fun testPart2() {
        val lines = readLines("day3")
        assertEquals(0, day3part2(lines))
    }
}