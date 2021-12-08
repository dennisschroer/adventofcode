package nl.dennisschroer.adventofcode.year2021

class Day8 {
    fun part1(input: List<String>): Int {
        return input.sumOf { it.substringAfter("| ").split(" ").count { it.length in listOf(2, 3, 4, 7) } }
    }

    fun part2(input: List<String>): Int {
        val lines: List<Pair<List<String>, List<String>>> = input.map { it.substringBefore(" |").split(" ") to it.substringAfter("| ").split(" ") }

        lines.forEach { line ->
            val patterns : List<String> = line.first

            // Target segments:
            //  aaaa
            // b    c
            // b    c
            //  dddd
            // e    f
            // e    f
            //  gggg
            //
            // Length of patterns:
            // 0 = 6
            // 1 = 2 *
            // 2 = 5
            // 3 = 5
            // 4 = 4 *
            // 5 = 5
            // 6 = 6
            // 7 = 3 *
            // 8 = 7 *
            // 9 = 6
            //
            // Amount each segment occurs
            // a = 8
            // b = 6 *
            // c = 8
            // d = 7
            // e = 4 *
            // f = 9 *
            // g = 7

            // Map of target segment to mapped segment
            val segments = mutableMapOf<Char, Char>()

            // Lets count each segment
            val segmentCounts = patterns.joinToString("").groupingBy { it }.eachCount()

            // The top segment is digit 7 minus digit 1
            segments['a'] = (patterns.first { it.length == 3 }.toSet() - patterns.first { it.length == 2 }.toSet()).first()

            // Set segments based on unique counts
            segments['f'] = segmentCounts.filter { it.value == 9 }.keys.first()
            segments['e'] = segmentCounts.filter { it.value == 4 }.keys.first()
            segments['b'] = segmentCounts.filter { it.value == 6 }.keys.first()

            // c
            // d
            // g

        }

        return -1
    }
}

fun main() {
    println("Part 1: ${Day8().part1(readLines("day8"))}")
    println("Part 2: ${Day8().part2(readLines("day8"))}")
}