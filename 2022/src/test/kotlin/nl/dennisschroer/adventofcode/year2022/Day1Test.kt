package nl.dennisschroer.adventofcode.year2022

import org.junit.jupiter.api.Test

import org.junit.jupiter.api.Assertions.*

class Day1Test {
    @Test
    fun part1() {
        assertEquals(24000, day1part1(readBlocks("day1")))
    }

    @Test
    fun part2() {
        assertEquals(45000, day1part2(readBlocks("day1")))
    }
}