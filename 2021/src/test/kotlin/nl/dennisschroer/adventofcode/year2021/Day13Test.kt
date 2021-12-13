package nl.dennisschroer.adventofcode.year2021

import org.junit.jupiter.api.Assertions.assertEquals
import org.junit.jupiter.api.Test

internal class Day13Test {

    @Test
    fun testPart1() {
        assertEquals(17, Day13().part1(readFile("testDay13")))
    }

    @Test
    fun testPart2() {
        Day13().part2(readFile("testDay13"))
    }
}