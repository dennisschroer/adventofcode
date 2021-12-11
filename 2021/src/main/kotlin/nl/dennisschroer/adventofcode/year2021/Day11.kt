package nl.dennisschroer.adventofcode.year2021

class Day11 {
    fun part1(input: List<String>): Int {
        // Grid[y,x]
        val grid = input.map { it.toList().map { it.digitToInt() } }

        return (0 until 100).fold(0 to grid) { state: Pair<Int, List<List<Int>>>, step ->
            println("==== Step $step ====")
            printState(state)
            val next = iterate(state.second)
            state.first + next.first to next.second
        }.first
    }

    private fun printState(state: Pair<Int, List<List<Int>>>) {
        println("Total flashes: ${state.first}")
        println(state.second.joinToString("\n") { it.joinToString("") }.replace("0", "\u001B[33m0\u001B[0m"))
    }

    private fun iterate(grid: List<List<Int>>): Pair<Int, List<List<Int>>> {
        var flashes = 0

        // Increase by 1
        var newGrid = grid.map { row -> row.map { it + 1 }.toMutableList() }.toMutableList()

        repeat(15) { // 15 seems to be enough

            (0..9).forEach { y ->
                (0..9).forEach { x ->
                    val value = newGrid[y][x]

                    // If flashing, increase neighbours by one
                    if (value in 10..999) {
                        if (y > 0) {
                            if (x > 0) newGrid[y - 1][x - 1] += 1
                            newGrid[y - 1][x] += 1
                            if (x < 9) newGrid[y - 1][x + 1] += 1
                        }
                        if (x > 0) newGrid[y][x - 1] += 1
                        if (x < 9) newGrid[y][x + 1] += 1
                        if (y < 9) {
                            if (x > 0) newGrid[y + 1][x - 1] += 1
                            newGrid[y + 1][x] += 1
                            if (x < 9) newGrid[y + 1][x + 1] += 1
                        }

                        // Mark as flashed
                        newGrid[y][x] = 1000
                    }
                }
            }
        }

        // Assert we are done
        assert(newGrid.map { row -> row.filter { it in 10..999 } }.flatten().isEmpty())

        // Count flashes
        flashes += newGrid.sumOf { row -> row.count { it > 9 } }

        // Reset flashed octopusses
        newGrid = newGrid.map { row -> row.map { if (it > 900) 0 else it }.toMutableList() }.toMutableList()

        return flashes to newGrid
    }

    fun part2(input: List<String>): Int {
        return -1
    }
}

fun main() {
    println("Part 1: ${Day11().part1(readLines("day11"))}")
    println("Part 2: ${Day11().part2(readLines("day11"))}")
}