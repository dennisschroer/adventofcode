package nl.dennisschroer.adventofcode.year2021

import org.junit.jupiter.api.Assertions.assertEquals
import org.junit.jupiter.api.Test

internal class Day8Test {

    @Test
    fun testPart1() {
        val lines = readLines("testDay8")
        assertEquals(26, Day8().part1(lines))
    }

    @Test
    fun testPart2() {
        val lines = readLines("testDay8")
        assertEquals(0, Day8().part2(lines))
    }
}