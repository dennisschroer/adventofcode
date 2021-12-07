package nl.dennisschroer.adventofcode.year2021

import kotlin.math.abs

class Day7 {
    fun part1(input: String): Int {
        val crabs : List<Int> = input.split(",").map { it.toInt() }
        val positions = (crabs.minOrNull()!!  until crabs.maxOrNull()!!).associateWith { position -> crabs.sumOf { abs(position - it) } }
        return positions.minByOrNull{it.value}!!.value
    }

    fun part2(input: String): Int {
        return -1
    }
}

fun main() {
    println("Part 1: ${Day7().part1(readLines("day7").first())}")
    println("Part 2: ${Day7().part2(readLines("day7").first())}")
}