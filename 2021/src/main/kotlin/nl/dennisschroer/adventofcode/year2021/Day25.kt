package nl.dennisschroer.adventofcode.year2021

class Day25 {
    fun part1(input: List<String>): Int {
        var map = input.map { it.toList() }
        var previousMap: List<List<Char>>
        var count = 0

        do {
            previousMap = map
            map = step(map)
            count++
        } while(map != previousMap)

        return count
    }

    fun step(map: List<List<Char>>): List<List<Char>> {
        // Horizontal steps
        val result1 = map.map { row ->
            (row.takeLast(1) + row + row.first()).windowed(3).map(this::moveRight)
        }

        // Transpose
        val transposed = transpose(result1)

        // Vertical steps
        val result2 = transposed.map { column ->
            (column.takeLast(1) + column + column.first()).windowed(3).map(this::moveDown)
        }

        return transpose(result2)
    }

    /**
     * Transpose the matrix (switch rows and columns).
     */
    private fun transpose(matrix: List<List<Char>>): List<List<Char>> = (0 until matrix[0].size).map { columnIndex -> matrix.map { it[columnIndex] } }

    /**
     * For a list of 3 characters determines the new character in the middle
     */
    private fun moveRight(window: List<Char>): Char {
        return if (window[0] == '>' && window[1] == '.') {
            '>'
        } else if (window[1] == '>' && window[2] == '.') {
            '.'
        } else {
            window[1]
        }
    }

    /**
     * For a list of 3 characters determines the new character in the middle
     */
    private fun moveDown(window: List<Char>): Char {
        return if (window[0] == 'v' && window[1] == '.') {
            'v'
        } else if (window[1] == 'v' && window[2] == '.') {
            '.'
        } else {
            window[1]
        }
    }

    fun part2(input: List<String>): Int {
        return -1
    }

    private fun printMap(map: List<List<Char>>) = map.forEach { println(it.joinToString("")) }
}

fun main() {
    println("Part 1: ${Day25().part1(readLines("day25"))}")
    println("Part 2: ${Day25().part2(readLines("day25"))}")
}