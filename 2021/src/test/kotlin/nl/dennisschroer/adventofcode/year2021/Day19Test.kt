package nl.dennisschroer.adventofcode.year2021

import org.junit.jupiter.api.Assertions.assertEquals
import org.junit.jupiter.api.Test

internal class Day19Test {

    @Test
    fun testPart1() {
        assertEquals(79, Day19().part1(readFile("testDay19")))
    }

    @Test
    fun testPart2() {
        assertEquals(3621, Day19().part2(readFile("testDay19")))
    }
}