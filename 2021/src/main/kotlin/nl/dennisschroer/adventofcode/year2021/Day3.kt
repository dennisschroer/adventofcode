package nl.dennisschroer.adventofcode.year2021

class Day3 {
    fun part1(input: List<String>): Int {
        return gammaRate(input).toInt(2) * epsilonRate(input).toInt(2)
    }

    fun part2(input: List<String>): Int {
        return oxygenGeneratorRating(input).toInt(2) * co2ScrubberRating(input).toInt(2)
    }

    fun gammaRate(input: List<String>): String {
        val length = input[0].length
        return (0 until length).map { index -> mostOccuringChar(input.map { it[index] }) }.joinToString("")
    }

    fun epsilonRate(input: List<String>): String {
        val length = input[0].length
        return (0 until length).map { index -> leastOccuringChar(input.map { it[index] }) }.joinToString("")
    }

    fun oxygenGeneratorRating(input: List<String>): String {
        var bitPosition = 0
        var remainingInput = input
        var mostOccuringChar: Char

        while (remainingInput.size > 1) {
            mostOccuringChar = mostOccuringChar(remainingInput.map { it[bitPosition] })
            remainingInput = remainingInput.filter { it[bitPosition] == mostOccuringChar }
            bitPosition++
        }

        return remainingInput.first()
    }

    fun co2ScrubberRating(input: List<String>): String {
        var bitPosition = 0
        var remainingInput = input
        var mostOccuringChar: Char

        while (remainingInput.size > 1) {
            mostOccuringChar = leastOccuringChar(remainingInput.map { it[bitPosition] })
            remainingInput = remainingInput.filter { it[bitPosition] == mostOccuringChar }
            bitPosition++
        }

        return remainingInput.first()
    }

    fun mostOccuringChar(input: List<Char>): Char {
        val counts = input.groupingBy { it }.eachCount()
        // For part 2: if both occurences are equal pick '1'
        return if (counts['0']!! > counts['1']!!) '0' else '1'
    }

    fun leastOccuringChar(input: List<Char>): Char {
        val counts = input.groupingBy { it }.eachCount()
        // For part 2: if both occurences are equal pick '0'
        return if (counts['0']!! <= counts['1']!!) '0' else '1'
    }
}

fun main() {
    println("Part 1: ${Day3().part1(readLines("day3"))}")
    println("Part 2: ${Day3().part2(readLines("day3"))}")
}