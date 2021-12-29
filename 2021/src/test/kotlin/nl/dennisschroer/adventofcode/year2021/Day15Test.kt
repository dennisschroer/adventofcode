package nl.dennisschroer.adventofcode.year2021

import org.junit.jupiter.api.Assertions.assertEquals
import org.junit.jupiter.api.Test

internal class Day15Test {

    @Test
    fun testPart1() {
        assertEquals(40, Day15().part1(readLines("testDay15")))
    }

    @Test
    fun testPart2() {
        assertEquals(315, Day15().part2(readLines("testDay15")))
    }
}