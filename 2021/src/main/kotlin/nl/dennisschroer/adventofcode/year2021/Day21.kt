package nl.dennisschroer.adventofcode.year2021

import kotlin.math.max
import kotlin.math.min

class Day21 {
    fun part1(startPosition1: Int, startPosition2: Int): Int {
        var die = 0
        var turn = 1
        var position1 = startPosition1
        var position2 = startPosition2
        var score1 = 0
        var score2 = 0

        while (score1 < 1000 && score2 < 1000) {
            val moves = (0 until 3).sumOf {
                die++
                ((die - 1) % 100) + 1
            }

            if (turn == 1) {
                position1 = (position1 + moves - 1) % 10 + 1
                score1 += position1
            } else {
                position2 = (position2 + moves - 1) % 10 + 1
                score2 += position2
            }

            turn = (turn % 2) + 1
        }

        return min(score1, score2) * die
    }

    fun part2(startPosition1: Int, startPosition2: Int): Long {
        val result = playGame(startPosition1, 0, startPosition2, 0)
        return max(result.first, result.second)
    }

    private fun playGame(startPosition1: Int, score1: Int, startPosition2: Int, score2: Int): Pair<Long, Long> {
        // Sum of dice to amount of universes
        // 3 = 1
        // 4 = 3
        // 5 = 6
        // 6 = 7
        // 7 = 6
        // 8 = 3
        // 9 = 1

        // Base case. In this method, player 1 never wins
        if (score2 > 20) {
            return 0L to 1L
        }

        // Recursive case
        return listOf(
            ((startPosition1 + 3 - 1) % 10 + 1).let { playGame(startPosition2, score2, it, score1 + it) },
            ((startPosition1 + 4 - 1) % 10 + 1).let { playGame(startPosition2, score2, it, score1 + it).let { (a, b) -> a * 3 to b * 3 } },
            ((startPosition1 + 5 - 1) % 10 + 1).let { playGame(startPosition2, score2, it, score1 + it).let { (a, b) -> a * 6 to b * 6 } },
            ((startPosition1 + 6 - 1) % 10 + 1).let { playGame(startPosition2, score2, it, score1 + it).let { (a, b) -> a * 7 to b * 7 } },
            ((startPosition1 + 7 - 1) % 10 + 1).let { playGame(startPosition2, score2, it, score1 + it).let { (a, b) -> a * 6 to b * 6 } },
            ((startPosition1 + 8 - 1) % 10 + 1).let { playGame(startPosition2, score2, it, score1 + it).let { (a, b) -> a * 3 to b * 3 } },
            ((startPosition1 + 9 - 1) % 10 + 1).let { playGame(startPosition2, score2, it, score1 + it) },
        )
            // Add up totals
            .reduce { acc, pair -> acc.first + pair.first to acc.second + pair.second }
            // Switch players back
            .let { it.second to it.first }
    }
}

fun main() {
    println("Part 1: ${Day21().part1(1, 3)}")
    println("Part 2: ${Day21().part2(1, 3)}")
}