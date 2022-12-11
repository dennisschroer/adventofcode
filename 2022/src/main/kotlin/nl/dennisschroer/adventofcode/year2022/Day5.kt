package nl.dennisschroer.adventofcode.year2022


fun day5part1(input: List<List<String>>): String {
    val rawStacks = input.first()
    val rawSteps = input.last()

    val stacks = parseStacks(rawStacks)
    val steps = parseSteps(rawSteps)

    // Debug: print stacks
    stacks.forEach { println(it) }

    val result = steps.fold(stacks) { stacks, step ->
        println(step)

        val newStacks = stacks.toMutableMap()
        newStacks[step.from] = stacks[step.from]!!.dropLast(step.count)
        newStacks[step.to] = stacks[step.to] + stacks[step.from]!!.takeLast(step.count).reversed()

        newStacks.forEach { println(it) }
        newStacks
    }

    // Get top one of each stack
    return result.values.fold("") { acc, stack -> acc + stack.last() }
}

data class Step(val count: Int, val from: Int, val to: Int)

fun parseSteps(input: List<String>): List<Step> = input.mapNotNull {
    val (count, from, to) = "move (\\d+) from (\\d+) to (\\d+)".toRegex().matchEntire(it)!!.destructured
    Step(count.toInt(), from.toInt(), to.toInt())
}

fun parseStacks(input: List<String>): Map<Int, String> {
    val parsedStacks = input
        // Only the middle character matters
        .map { line -> line.windowed(4, 4, true).map { it[1] } }
        // Drop last line
        .dropLast(1)
    val stackCount = parsedStacks.maxOf { it.size }

    // Transpose into stacks
    val stacks = mutableMapOf<Int, String>()
    for (i in 0 until stackCount) {
        stacks[i + 1] =
            parsedStacks.foldRight("") { row, acc -> if (i < row.size && row[i] != ' ') "$acc${row[i]}" else acc }
    }

    return stacks
}

fun day5part2(input: List<List<String>>): Int {
    return -1
}

fun main() {
    println("Part 1: ${day5part1(readBlocks("day5"))}")
    println("Part 2: ${day5part2(readBlocks("day5"))}")
}