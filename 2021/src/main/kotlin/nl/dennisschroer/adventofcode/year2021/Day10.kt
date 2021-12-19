package nl.dennisschroer.adventofcode.year2021

import java.lang.Exception

class Day10 {
    fun part1(input: List<String>): Int {
        return input.map { line ->
            // Golf away all closed pairs
            (0..line.length).fold(line) { it, _ ->
                it.replace("()", "")
                    .replace("[]", "")
                    .replace("<>", "")
                    .replace("{}", "")
            }

                // Remove all starting brackets
                .replace("[(\\[<{]".toRegex(), "")

            // Filter those not empty
        }.filter { it.isNotEmpty() }

            // Take first char
            .map { it[0] }

            // Calculate score
            .map {
                when (it) {
                    ')' -> 3
                    ']' -> 57
                    '}' -> 1197
                    '>' -> 25137
                    else -> throw Exception()
                }
            }.sum()
    }

    fun part2(input: List<String>): Int {
        return -1
    }
}

fun main() {
    println("Part 1: ${Day10().part1(readLines("day10"))}")
    println("Part 2: ${Day10().part2(readLines("day10"))}")
}