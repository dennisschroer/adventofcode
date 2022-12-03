package nl.dennisschroer.adventofcode.year2022

import org.junit.jupiter.api.Test

import org.junit.jupiter.api.Assertions.*

class Day2Test {
    @Test
    fun part1() {
        assertEquals(15, day2part1(readLines("day2")))
    }

    @Test
    fun part2() {
        assertEquals(0, day2part2(readLines("day2")))
    }
}