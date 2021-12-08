package nl.dennisschroer.adventofcode.year2021

class Day8 {
    fun part1(input: List<String>): Int {
        return input.map { it.substringAfter("|").trim().split(" ").filter { it.length in listOf(2,3,4,7) }.count() }.sum()
    }

    fun part2(input: List<String>): Int {
        return -1
    }
}

fun main() {
    println("Part 1: ${Day8().part1(readLines("day8"))}")
    println("Part 2: ${Day8().part2(readLines("day8"))}")
}