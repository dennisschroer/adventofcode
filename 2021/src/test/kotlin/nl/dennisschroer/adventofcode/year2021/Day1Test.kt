package nl.dennisschroer.adventofcode.year2021

import org.junit.jupiter.api.Assertions.assertEquals
import org.junit.jupiter.api.Test

internal class Day1Test {

    @Test
    fun testPart1() {
        assertEquals(7, Day1().part1(readLines("testDay1")))
    }

    @Test
    fun testPart2() {
        assertEquals(5, Day1().part2(readLines("testDay1")))
    }
}