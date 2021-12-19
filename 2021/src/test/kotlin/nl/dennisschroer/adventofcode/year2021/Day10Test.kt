package nl.dennisschroer.adventofcode.year2021

import org.junit.jupiter.api.Assertions.assertEquals
import org.junit.jupiter.api.Test

internal class Day10Test {

    @Test
    fun testPart1() {
        assertEquals(26397, Day10().part1(readLines("testDay10")))
    }

    @Test
    fun testPart2() {
        assertEquals(288957, Day10().part2(readLines("testDay10")))
    }
}