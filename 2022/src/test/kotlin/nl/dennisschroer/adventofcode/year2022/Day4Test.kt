package nl.dennisschroer.adventofcode.year2022

import org.junit.jupiter.api.Assertions.assertEquals
import org.junit.jupiter.api.Test

internal class Day4Test {

    @Test
    fun testPart1() {
        val lines = readLines("day4")
        assertEquals(2, day4part1(lines))
    }

    @Test
    fun testPart2() {
        val lines = readLines("day4")
        assertEquals(4, day4part2(lines))
    }
}