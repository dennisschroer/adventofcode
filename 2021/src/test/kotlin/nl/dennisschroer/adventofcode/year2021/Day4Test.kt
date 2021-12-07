package nl.dennisschroer.adventofcode.year2021

import org.junit.jupiter.api.Assertions.assertEquals
import org.junit.jupiter.api.Test

internal class Day4Test {

    @Test
    fun testPart1() {
        val lines = readFile("testDay4")

        assertEquals(4512, Day4().part1(lines))
    }

    @Test
    fun testPart2() {
        val lines = readFile("testDay4")

        assertEquals(-1, Day4().part2(lines))
    }
}