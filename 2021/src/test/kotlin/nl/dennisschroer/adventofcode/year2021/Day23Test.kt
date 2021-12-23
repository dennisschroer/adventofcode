package nl.dennisschroer.adventofcode.year2021

import org.junit.jupiter.api.Assertions.assertEquals
import org.junit.jupiter.api.Test

internal class Day23Test {

    @Test
    fun testPart1() {
        assertEquals(12521, Day23().part1(".......BACDBCDA".toList()))
    }

    @Test
    fun testPart2() {
        assertEquals(0, Day23().part2(".......BACDBCDA".toList()))
    }
}