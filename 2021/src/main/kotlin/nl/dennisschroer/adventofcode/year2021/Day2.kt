package nl.dennisschroer.adventofcode.year2021

class Day2 {
    fun part1(input: List<String>): Int {
        val commands: List<Pair<String, Int>> = input.map { it.split(" ") }.map { it[0] to it[1].toInt() }

        val depth = commands.sumOf {
            when (it.first) {
                "down" -> it.second
                "up" -> -it.second
                else -> 0
            }
        }

        val horizontal = commands.filter { it.first == "forward" }.sumOf { it.second }

        return depth * horizontal
    }

    fun part2(input: List<String>): Int {
        return 0
    }
}

fun main() {
    println("Part 1: ${Day2().part1(readLines("day2"))}")
    println("Part 2: ${Day2().part2(readLines("day2"))}")
}