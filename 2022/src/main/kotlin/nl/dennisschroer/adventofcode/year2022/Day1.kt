package nl.dennisschroer.adventofcode.year2022

fun day1part1(blocks: List<List<String>>): Int {
    return blocks.maxOf { lines -> lines.sumOf { it.toInt() } }
}

fun day1part2(blocks: List<List<String>>): Int {
    return blocks.map { lines -> lines.sumOf { it.toInt() } }.sorted().takeLast(3).sum()
}

fun main() {
    println("Part 1: ${day1part1(readBlocks("day1"))}")
    println("Part 2: ${day1part2(readBlocks("day1"))}")
}