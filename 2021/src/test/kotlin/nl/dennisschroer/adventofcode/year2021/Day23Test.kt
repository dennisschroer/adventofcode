package nl.dennisschroer.adventofcode.year2021

import org.junit.jupiter.api.Assertions.assertEquals
import org.junit.jupiter.api.Test

internal class Day23Test {

    @Test
    fun testPart1() {
        assertEquals(12521, Day23().part1(Day23.Game(".......", "BA", "CD", "BC", "DA", 0)))
    }

    @Test
    fun testPart2() {
        assertEquals(44169, Day23().part2(Day23.Game(".......", "BDDA", "CCBD", "BBAC", "DACA", 0)))
    }
}