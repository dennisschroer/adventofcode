package nl.dennisschroer.adventofcode.year2021

import org.junit.jupiter.api.Assertions.assertEquals
import org.junit.jupiter.api.Test

internal class Day9Test {

    @Test
    fun testPart1() {
        assertEquals(15, Day9().part1(readLines("testDay9")))
    }

    @Test
    fun testPart2() {
        assertEquals(1134, Day9().part2(readLines("testDay9")))
    }
}