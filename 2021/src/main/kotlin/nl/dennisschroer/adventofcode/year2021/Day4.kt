package nl.dennisschroer.adventofcode.year2021

class Day4 {
    data class BingoGame(val numbers: List<Int>, val cards: List<BingoCard>)

    data class BingoCard(val grid: List<List<Int>>) {
        private fun rowsAndColumns(): List<List<Int>> = grid + (0 until 5).map { column -> grid.map { it[column] } }

        fun hasBingo(numbers: List<Int>): Boolean {
            return rowsAndColumns().any { numbers.containsAll(it) }
        }
    }

    fun part1(input: String): Int {
        val game = parseInput(input)
        var winningCard: BingoCard? = null
        var progress = 0

        while (winningCard == null) {
            progress++
            winningCard = game.cards.firstOrNull { it.hasBingo(game.numbers.take(progress)) }
        }

        val winningNumber = game.numbers[progress - 1]
        val unmarkedNumbersSum = (winningCard.grid.flatten() - game.numbers.take(progress).toSet()).sum()

        return winningNumber * unmarkedNumbersSum
    }

    private fun parseInput(input: String): BingoGame {
        val parts = input.split("\n\n")
        val numbers = parts[0].trim().split(",").map { it.toInt() }
        val cards = parts.drop(1).map { grid ->
            BingoCard(grid.trim().split("\n").map { row ->
                row.trim().split("\\s+".toRegex()).map { it.toInt() }
            })
        }

        return BingoGame(numbers, cards)
    }

    fun part2(input: String): Int {
        return 0
    }
}

fun main() {
    println("Part 1: ${Day4().part1(readFile("day4"))}")
    println("Part 2: ${Day4().part2(readFile("day4"))}")
}