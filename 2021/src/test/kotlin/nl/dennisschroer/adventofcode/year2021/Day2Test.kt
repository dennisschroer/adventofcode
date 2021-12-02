package nl.dennisschroer.adventofcode.year2021

import org.junit.jupiter.api.Assertions.assertEquals
import org.junit.jupiter.api.Test

internal class Day2Test {

    @Test
    fun testPart1() {
        assertEquals(150, Day2().part1(readLines("testDay2")))
    }

    @Test
    fun testPart2() {
        assertEquals(900, Day2().part2(readLines("testDay2")))
    }
}