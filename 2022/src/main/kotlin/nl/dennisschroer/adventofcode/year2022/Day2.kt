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
    return lines.map {
        it
            .replace("A", "R")
            .replace("B", "P")
            .replace("C", "S")
            .replace("X", "L")
            .replace("Y", "D")
            .replace("Z", "W")
    }.map {
        when (it) {
            "R L" -> 3 + 0
            "R D" -> 1 + 3
            "R W" -> 2 + 6
            "P L" -> 1 + 0
            "P D" -> 2 + 3
            "P W" -> 3 + 6
            "S L" -> 2 + 0
            "S D" -> 3 + 3
            "S W" -> 1 + 6
            else -> error("Not valid")
        }
    }.sum()
}

fun main() {
    println("Part 1: ${day2part1(readLines("day2"))}")
    println("Part 2: ${day2part2(readLines("day2"))}")
}