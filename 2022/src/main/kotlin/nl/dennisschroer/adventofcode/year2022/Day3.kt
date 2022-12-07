package nl.dennisschroer.adventofcode.year2022

fun day3part1(input: List<String>): Int {
    return input
        // Split in two halves
        .map { it.substring(0, it.length / 2) to it.substring(it.length / 2) }
        // Intersect two halves to find common character
        .map { (first, second) -> first.toSet().intersect(second.toSet()) }
        // Map to one character
        .map { it.first() }
        // Calculates priority
        .sumOf { priority(it) }
}

fun priority(char: Char): Int = when (char) {
    in 'a'..'z' -> char - 'a' + 1
    in 'A'..'Z' -> char - 'A' + 27
    else -> error("Invalid character $char")
}

fun day3part2(input: List<String>): Int {
    return input
        // Split in groups of 3
        .windowed(3, 3)
        // Find badge (common character in all strings)
        .map { group -> group.reduce { a, b -> a.toSet().intersect(b.toSet()).joinToString() } }
        // Map to one character
        .map { it.first() }
        // Calculate priority
        .sumOf { priority(it) }
}

fun main() {
    println("Part 1: ${day3part1(readLines("day3"))}")
    println("Part 2: ${day3part2(readLines("day3"))}")
}