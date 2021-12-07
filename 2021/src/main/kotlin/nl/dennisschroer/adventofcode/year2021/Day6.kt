package nl.dennisschroer.adventofcode.year2021

class Day6 {
    fun part1(input: String): Long {
        val fishes = input.split(",").map { it.toInt() }.toMutableList()
        val fishCounts = (0..8).associateWith { c -> fishes.count { it == c }.toLong() }.toMutableMap()
        repeat(80) { iterateDay(fishCounts) }
        return fishCounts.values.sum()
    }

    fun part2(input: String): Long {
        val fishes = input.split(",").map { it.toInt() }.toMutableList()
        val fishCounts = (0..8).associateWith { c -> fishes.count { it == c }.toLong() }.toMutableMap()
        repeat(256) { iterateDay(fishCounts) }
        return fishCounts.values.sum()
    }

    private fun iterateDay(fishCounts: MutableMap<Int, Long>) {
        val newFishes = fishCounts[0]!!

        (0..7).forEach { fishCounts[it] = fishCounts[it+1]!! }
        fishCounts[8] = newFishes // New fishes are born
        fishCounts[6] =  fishCounts[6]!! + newFishes // Parent fishes are reset to 6
    }
}

fun main() {
    println("Part 1: ${Day6().part1(readLines("day6")[0])}")
    println("Part 2: ${Day6().part2(readLines("day6")[0])}")
}