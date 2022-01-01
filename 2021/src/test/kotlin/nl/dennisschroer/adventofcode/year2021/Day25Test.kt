package nl.dennisschroer.adventofcode.year2021

import org.junit.jupiter.api.Assertions.assertEquals
import org.junit.jupiter.api.Test

internal class Day25Test {

    private val d = Day25()

    @Test
    fun testStepRight() {
        assertEquals(listOf("...>>>>.>..".toList()), d.step(listOf("...>>>>>...".toList())))
        assertEquals(listOf("...>>>.>.>.".toList()), d.step(listOf("...>>>>.>..".toList())))
    }

    @Test
    fun testStepBothHerds() {
        val map = """
            ..........
            .>v....v..
            .......>..
            ..........
        """.trimIndent().split("\n").map { it.toList() }

        val expected = """
            ..........
            .>........
            ..v....v>.
            ..........
        """.trimIndent().split("\n").map { it.toList() }

        assertEquals(expected, d.step(map))
    }

    @Test
    fun testPart1() {
        assertEquals(58, d.part1(readLines("testDay25")))
    }

    @Test
    fun testPart2() {
        assertEquals(0, d.part2(readLines("testDay25")))
    }
}