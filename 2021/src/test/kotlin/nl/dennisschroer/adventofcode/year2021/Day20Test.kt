package nl.dennisschroer.adventofcode.year2021

import org.junit.jupiter.api.Assertions.assertEquals
import org.junit.jupiter.api.Test

internal class Day20Test {

    @Test
    fun testPart1() {
        assertEquals(35, Day20().part1(readFile("testDay20")))
    }

    @Test
    fun testPart2() {
        assertEquals(3351, Day20().part2(readFile("testDay20")))
    }
}