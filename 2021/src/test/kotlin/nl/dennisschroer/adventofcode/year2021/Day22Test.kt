package nl.dennisschroer.adventofcode.year2021

import org.junit.jupiter.api.Assertions.assertEquals
import org.junit.jupiter.api.Test

internal class Day22Test {

    @Test
    fun testPart1() {
        assertEquals(590784, Day22().part1(readLines("testDay22")))
    }

    @Test
    fun testPart2() {
        assertEquals(0, Day22().part2(readLines("testDay22")))
    }
}