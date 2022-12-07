package nl.dennisschroer.adventofcode.year2022

fun day4part1(input: List<String>): Int {
    return input
        // Split on comma
        .map { it.substringBefore(",") to it.substringAfter(",") }
        // Parse to ranges
        .map { parseRange(it.first) to parseRange(it.second) }
        // Keep only those which fully contain each other
        .count { it.first.fullyContains(it.second) || it.second.fullyContains(it.first) }
}

fun parseRange(range: String): IntRange =
    range.substringBefore("-").toInt()..range.substringAfter("-").toInt()

fun day4part2(input: List<String>): Int {
    return -1
}

fun main() {
    println("Part 1: ${day4part1(readLines("day4"))}")
    println("Part 2: ${day4part2(readLines("day4"))}")
}