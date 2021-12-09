package nl.dennisschroer.adventofcode.year2021

import org.junit.jupiter.api.Assertions.assertEquals
import org.junit.jupiter.api.Test

internal class Day8Test {

    @Test
    fun testPart1() {
        val lines = readLines("testDay8")
        assertEquals(26, Day8().part1(lines))
    }

    @Test
    fun testPart2() {
        val lines = readLines("testDay8")

        assertEquals(
            mapOf('a' to 'd', 'b' to 'e', 'c' to 'a', 'd' to 'f', 'e' to 'g', 'f' to 'b', 'g' to 'c'),
            Day8().resolveSegments("acedgfb cdfbe gcdfa fbcad dab cefabd cdfgeb eafb cagedb ab".split(" "))
        )

        assertEquals(61229, Day8().part2(lines))
    }
}