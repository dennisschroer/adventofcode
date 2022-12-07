package nl.dennisschroer.adventofcode.year2022

fun day5part1(input: List<List<String>>): String {
    val rawStacks = input.first()
    val rawSteps = input.last()

    val parsedStacks = rawStacks
        // Only the middle character matters
        .map { line -> line.windowed(4, 4, true).map { it[1] } }
        // Drop last line
        .dropLast(1)
    val stackCount = parsedStacks.maxOf { it.size }

    // Transpose into stacks
    val stacks = mutableMapOf<Int, String>()
    for (i in 0 until stackCount) {
        stacks[i+1] = parsedStacks.foldRight("") { row, acc -> if (i < row.size) "$acc${row[i]}" else acc }
    }

    // Debug: print stacks
    stacks.forEach { println(it) }



    return ""
}

fun day5part2(input: List<List<String>>): Int {
    return -1
}

fun main() {
    println("Part 1: ${day5part1(readBlocks("day5"))}")
    println("Part 2: ${day5part2(readBlocks("day5"))}")
}