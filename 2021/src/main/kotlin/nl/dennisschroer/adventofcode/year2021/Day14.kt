package nl.dennisschroer.adventofcode.year2021

class Day14 {
    fun part1(input: String): Int {
        val (polymer, insertions) = parseInput(input)

        val result = (0 until 10).fold(polymer) { step, _ -> insert(step, insertions) }
        val counts = result.groupingBy { it }.eachCount()

        return counts.maxOf { it.value } - counts.minOf { it.value }
    }

    private fun insert(polymer: String, insertions: Map<String, String>): String {
        return polymer.zipWithNext { a, b -> insertions.getOrDefault("$a$b", "$a") }.joinToString("") + polymer.last()
    }

    fun part2(input: String): Long {
        val polymer = input.substringBefore("\n\n")

        // Each pair is replaced by two pairs
        val replacements = input.substringAfter("\n\n").split("\n").associate { it.take(2) to listOf("${it[0]}${it.last()}", "${it.last()}${it[1]}") }

        // Split polymer to pairs
        val pairs = polymer.zipWithNext { a, b -> "$a$b" }

        // Count the pairs
        val counts = pairs.groupingBy { it }.eachCount().mapValues { it.value.toLong() }

        // Now repeatedly replace pairs
        val result = (0 until 40).fold(counts) { stepCounts, _ ->
            val nextStep = mutableMapOf<String, Long>()
            stepCounts.forEach { (pair, count) ->
                replacements.getOrDefault(pair, listOf(pair)).forEach {
                    nextStep[it] = nextStep.getOrDefault(it, 0) + count
                }
            }
            nextStep
        }

        // Reduce pairs to singles by removing the second character of the pair
        val characterCounts =
            result.toList().map { it.first.first() to it.second }.groupingBy { it.first }.fold(0L) { a, b -> a + b.second }.toMutableMap()

        // We missed the very last character
        characterCounts[polymer.last()] = characterCounts[polymer.last()]!! + 1

        // Return difference
        return characterCounts.maxOf { it.value } - characterCounts.minOf { it.value }
    }

    fun parseInput(input: String): Pair<String, Map<String, String>> {
        val polymer = input.substringBefore("\n\n")
        val insertions = input.substringAfter("\n\n").split("\n").associate { it.take(2) to "${it[0]}${it.last()}" }

        return polymer to insertions
    }
}

fun main() {
    println("Part 1: ${Day14().part1(readFile("day14"))}")
    println("Part 2: ${Day14().part2(readFile("day14"))}")
}