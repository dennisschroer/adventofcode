package nl.dennisschroer.adventofcode.year2021

class Day1 {
    fun part1(input: List<Int>): Int {
        return input.zipWithNext { a, b -> b - a }.count { it > 0 }
    }

    fun part2(input: List<Int>): Int {
        return (0..input.size - 3).map { input.subList(it, it + 3).sum() }.zipWithNext { a, b -> b - a }.count { it > 0 }
    }
}

fun main() {
    println("Part 1: ${Day1().part1(readNumbers("day1"))}")
    println("Part 2: ${Day1().part2(readNumbers("day1"))}")
}