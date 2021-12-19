package nl.dennisschroer.adventofcode.year2021

import org.junit.jupiter.api.Assertions.assertEquals
import org.junit.jupiter.api.Test

internal class Day17Test {

    @Test
    fun testPart1() {
        assertEquals(45, Day17().part1(20..30, -10..-5))
    }

    @Test
    fun testPart2() {
        assertEquals(112, Day17().part2(20..30, -10..-5))
    }
}