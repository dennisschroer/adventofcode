package nl.dennisschroer.adventofcode.year2021

class Day14 {
    fun part1(input: String): Int {
        val (polymer, insertions) = parseInput(input)

        val result = (0 until 10).fold(polymer) { step, _ -> insert(step, insertions) }
        val counts = result.groupingBy { it }.eachCount()

        return counts.maxOf { it.value } - counts.minOf { it.value }
    }

    private fun insert(polymer: String, insertions: Map<String, String>): String {
        val result = polymer.zipWithNext { a, b -> insertions.getOrDefault("$a$b", "$a") }.joinToString("") + polymer.last()
        println(result)
        return result
    }

    fun part2(input: String): Int {
        return -1
    }

    fun parseInput(input: String): Pair<String, Map<String, String>> {
        val polymer = input.substringBefore("\n\n")
        val insertions = input.substringAfter("\n\n").split("\n").associate { it.take(2) to "${it[0]}${it.last()}" }

        return polymer to insertions
    }
}

fun main() {
    println("Part 1: ${Day14().part1(readFile("day14"))}")
    println("Part 2: ${Day14().part2(readFile("day14"))}")
}