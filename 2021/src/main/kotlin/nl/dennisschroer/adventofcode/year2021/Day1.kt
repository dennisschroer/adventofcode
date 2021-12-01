package nl.dennisschroer.adventofcode.year2021

class Day1 {
    fun part1(input: List<String>): Int {
        return input.map { it.toInt() }.zipWithNext { a, b -> b - a }.count { it > 0 }
    }
}

fun main() {
    println("Part 1: ${Day1().part1(readLines("day1"))}")
}