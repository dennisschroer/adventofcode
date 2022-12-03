package nl.dennisschroer.adventofcode.year2022

fun day2part1(lines: List<String>): Int {
    return lines.map {
        it
            .replace("A", "R")
            .replace("B", "P")
            .replace("C", "S")
            .replace("X", "R")
            .replace("Y", "P")
            .replace("Z", "S")
    }.map {
        when (it) {
            "R R" -> 3 + 1
            "R P" -> 6 + 2
            "R S" -> 0 + 3
            "P R" -> 0 + 1
            "P P" -> 3 + 2
            "P S" -> 6 + 3
            "S R" -> 6 + 1
            "S P" -> 0 + 2
            "S S" -> 3 + 3
            else -> error("Not valid")
        }
    }.sum()
}

fun day2part2(lines: List<String>): Int {
    return -1
}

fun main() {
    println("Part 1: ${day2part1(readLines("day2"))}")
    println("Part 2: ${day2part2(readLines("day2"))}")
}