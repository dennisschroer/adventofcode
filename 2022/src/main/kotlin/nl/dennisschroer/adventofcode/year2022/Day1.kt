package nl.dennisschroer.adventofcode.year2022

fun day1part1(blocks: List<List<String>>): Int {
    return blocks.maxOf { lines -> lines.sumOf { it.toInt() } }
}

fun main() {
    println("Part 1: ${day1part1(readBlocks("day1"))}")
}