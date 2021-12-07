package nl.dennisschroer.adventofcode.year2021

import org.junit.jupiter.api.Assertions.assertEquals
import org.junit.jupiter.api.Test

internal class Day7Test {

    @Test
    fun testPart1() {
        val lines = readLines("testDay7")
        assertEquals(37, Day7().part1(lines.first()))
    }

    @Test
    fun testPart2() {
        val lines = readLines("testDay7")
        assertEquals(0, Day7().part2(lines.first()))
    }
}