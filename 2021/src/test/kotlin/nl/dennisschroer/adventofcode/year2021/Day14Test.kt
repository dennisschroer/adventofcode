package nl.dennisschroer.adventofcode.year2021

import org.junit.jupiter.api.Assertions.assertEquals
import org.junit.jupiter.api.Test

internal class Day14Test {

    @Test
    fun testPart1() {
        assertEquals(1588, Day14().part1(readFile("testDay14")))
    }

    @Test
    fun testPart2() {
        assertEquals(0, Day14().part2(readFile("testDay14")))
    }
}