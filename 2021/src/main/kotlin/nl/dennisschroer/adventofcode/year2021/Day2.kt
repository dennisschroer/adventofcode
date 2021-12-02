package nl.dennisschroer.adventofcode.year2021

class Day2 {
    fun part1(input: List<String>): Int {
        val commands = parseCommands(input)

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

    data class Position(var aim: Int = 0, var depth: Int = 0, var horizontal: Int = 0)

    fun part2(input: List<String>): Int {
        val commands = parseCommands(input)

        val position = commands.fold(Position()) { currentPosition, command ->
            when (command.first) {
                "down" -> currentPosition.apply { aim += command.second }
                "up" -> currentPosition.apply { aim -= command.second }
                else -> currentPosition.apply { horizontal += command.second; depth += command.second * aim }
            }
        }

        return position.depth * position.horizontal
    }

    private fun parseCommands(input: List<String>): List<Pair<String, Int>> = input.map { it.split(" ") }.map { it[0] to it[1].toInt() }
}

fun main() {
    println("Part 1: ${Day2().part1(readLines("day2"))}")
    println("Part 2: ${Day2().part2(readLines("day2"))}")
}