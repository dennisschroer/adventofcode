package nl.dennisschroer.adventofcode.year2021

import java.lang.Exception

class Day23 {
    /*
     *      Map of the game
     * #########################
     * # 0 1 . 2 . 3 . 4 . 5 6 #
     * ##### 7 # 9 # 11# 13#####
     *     # 8 # 10# 12# 14#
     *     #################
     */
    data class Game(val state: String, val energy: Int)

    private val COMPLETED_STATE: String = ".......AABBCCDD"

    data class Rule(val room: Int, val hallway: Int, val blockers: List<Int>, val distance: Int) {
        /**
         * Is the move from the room to the hallway allowed?
         */
        fun isAllowed(game: Game): Boolean {
            // room should not be empty
            return game.state[room] != '.' &&
                    // hallway should be empty
                    game.state[hallway] == '.' &&
                    // everything in between should be empty
                    blockers.all { game.state[it] == '.' } &&
                    // Do not allow "finished" amphipods to move
                    when (room) {
                        7 -> game.state[7] != 'A' || game.state[8] != 'A'
                        8 -> game.state[8] != 'A'
                        9 -> game.state[9] != 'B' || game.state[10] != 'B'
                        10 -> game.state[10] != 'B'
                        11 -> game.state[11] != 'C' || game.state[12] != 'C'
                        12 -> game.state[12] != 'C'
                        13 -> game.state[13] != 'D' || game.state[14] != 'D'
                        14 -> game.state[14] != 'D'
                        else -> true
                    }
        }

        /**
         * Is the move from the hallway to the room allowed?
         */
        fun isinverseAllowed(game: Game): Boolean {
            return game.state[room] == '.' && blockers.all { game.state[it] == '.' } &&
                    when (room) {
                        7 -> game.state[hallway] == 'A' && game.state[8] == 'A'
                        8 -> game.state[hallway] == 'A'
                        9 -> game.state[hallway] == 'B' && game.state[10] == 'B'
                        10 -> game.state[hallway] == 'B'
                        11 -> game.state[hallway] == 'C' && game.state[12] == 'C'
                        12 -> game.state[hallway] == 'C'
                        13 -> game.state[hallway] == 'D' && game.state[14] == 'D'
                        14 -> game.state[hallway] == 'D'
                        else -> false
                    }
        }
    }

    val rules = listOf(
        Rule(7, 0, listOf(1), 3),
        Rule(7, 1, listOf(), 2),
        Rule(7, 2, listOf(), 2),
        Rule(7, 3, listOf(2), 4),
        Rule(7, 4, listOf(2, 3), 6),
        Rule(7, 5, listOf(2, 3, 4), 8),
        Rule(7, 6, listOf(2, 3, 4, 5), 9),

        Rule(8, 0, listOf(7, 1), 4),
        Rule(8, 1, listOf(7), 3),
        Rule(8, 2, listOf(7), 3),
        Rule(8, 3, listOf(7, 2), 5),
        Rule(8, 4, listOf(7, 2, 3), 7),
        Rule(8, 5, listOf(7, 2, 3, 4), 8),
        Rule(8, 6, listOf(7, 2, 3, 4, 5), 10),

        Rule(9, 0, listOf(1, 2), 5),
        Rule(9, 1, listOf(2), 4),
        Rule(9, 2, listOf(), 2),
        Rule(9, 3, listOf(), 2),
        Rule(9, 4, listOf(3), 4),
        Rule(9, 5, listOf(3, 4), 6),
        Rule(9, 6, listOf(3, 4, 5), 7),

        Rule(10, 0, listOf(9, 1, 2), 6),
        Rule(10, 1, listOf(9, 2), 5),
        Rule(10, 2, listOf(9), 3),
        Rule(10, 3, listOf(9), 3),
        Rule(10, 4, listOf(9, 3), 5),
        Rule(10, 5, listOf(9, 3, 4), 7),
        Rule(10, 6, listOf(9, 3, 4, 5), 8),

        Rule(11, 0, listOf(1, 2, 3), 7),
        Rule(11, 1, listOf(2, 3), 6),
        Rule(11, 2, listOf(3), 4),
        Rule(11, 3, listOf(), 2),
        Rule(11, 4, listOf(), 2),
        Rule(11, 5, listOf(4), 4),
        Rule(11, 6, listOf(4, 5), 5),

        Rule(12, 0, listOf(11, 1, 2, 3), 8),
        Rule(12, 1, listOf(11, 2, 3), 7),
        Rule(12, 2, listOf(11, 3), 5),
        Rule(12, 3, listOf(11), 3),
        Rule(12, 4, listOf(11), 3),
        Rule(12, 5, listOf(11, 4), 5),
        Rule(12, 6, listOf(11, 4, 5), 6),

        Rule(13, 0, listOf(1, 2, 3, 4), 9),
        Rule(13, 1, listOf(2, 3, 4), 8),
        Rule(13, 2, listOf(3, 4), 6),
        Rule(13, 3, listOf(4), 4),
        Rule(13, 4, listOf(), 2),
        Rule(13, 5, listOf(), 2),
        Rule(13, 6, listOf(5), 3),

        Rule(14, 0, listOf(13, 1, 2, 3, 4), 10),
        Rule(14, 1, listOf(13, 2, 3, 4), 9),
        Rule(14, 2, listOf(13, 3, 4), 7),
        Rule(14, 3, listOf(13, 4), 5),
        Rule(14, 4, listOf(13), 3),
        Rule(14, 5, listOf(13), 3),
        Rule(14, 6, listOf(13, 5), 4),
    )

    fun printGame(game: Game) {
        print(game.state.take(7).toCharArray().joinToString(" "))
        println("   Energy ${game.energy}")
        print("   ")
        println(listOf(7, 9, 11, 13).map { game.state[it] }.joinToString(" "))
        print("   ")
        println(listOf(8, 10, 12, 14).map { game.state[it] }.joinToString(" "))
    }

    private fun reduceSolutions(games: List<Game>): List<Game> {
        if (games.isEmpty()) return games

        val completedGames = games.filter { it.state == COMPLETED_STATE }
        val uncompletedGames = games.filterNot { it.state == COMPLETED_STATE }

        val bestCompletedGame = completedGames.minByOrNull { it.energy }
        val maxScore = uncompletedGames.maxOfOrNull { completionScore(it) } ?: 0
        // Some magic threshold, just keep lowering it until it works
        val bestUncompletedGames = uncompletedGames
            .filter { it.energy >= (bestCompletedGame?.energy ?: 0) }
            .filter { completionScore(it) >= maxScore * 3 / 4 } // Some magic threshold to reduce amount of possible solutions

        println("${games.size} games contain ${completedGames.size} completed games and ${uncompletedGames.size} uncompleted games (reduced to ${bestUncompletedGames.size}, maxScore=$maxScore)")

        // Keep only the best completed solution plus all promising uncompleted games
        if (bestCompletedGame != null) {
            return listOf(bestCompletedGame) + bestUncompletedGames
        }
        return bestUncompletedGames
    }

    private fun completionScore(game: Game): Int {
        return (listOf(
            game.state[7] == 'A' && game.state[8] == 'A',
            game.state[8] == 'A',
            game.state[9] == 'B' && game.state[10] == 'B',
            game.state[10] == 'B',
            game.state[11] == 'C' && game.state[12] == 'C',
            game.state[12] == 'C',
            game.state[13] == 'D' && game.state[14] == 'D',
            game.state[14] == 'D',
        ).count { it } + 1) * 100000 - game.energy
    }

    /**
     * Play a single round executing all possible rules and returning all possible next game states.
     */
    private fun doRound(game: Game): List<Game> {
        // If it is completed keep it as a solution
        if (game.state == COMPLETED_STATE) return listOf(game)

        val allowedRules = rules.filter { it.isinverseAllowed(game) }
        return if (allowedRules.isNotEmpty()) {
            // Optimalization: if one amphipod can be moved in the room, always prefer to do this first
            return listOf(applyInverseRule(game, allowedRules[0]))
        } else {
            rules.filter { it.isAllowed(game) }.map { rule -> applyRule(game, rule) }
        }
    }

    private fun applyRule(game: Game, rule: Rule): Game {
        return Game(
            game.state.toMutableList().also {
                it[rule.hallway] = game.state[rule.room]
                it[rule.room] = '.'
            }.joinToString(""),
            game.energy + (scoreForLetter(game.state[rule.room]) * rule.distance)
        )
    }

    private fun applyInverseRule(game: Game, rule: Rule): Game {
        return Game(
            game.state.toMutableList().also {
                it[rule.room] = game.state[rule.hallway]
                it[rule.hallway] = '.'
            }.joinToString(""),
            game.energy + (scoreForLetter(game.state[rule.hallway]) * rule.distance)
        )
    }

    private fun scoreForLetter(c: Char): Int {
        return when (c) {
            'A' -> 1
            'B' -> 10
            'C' -> 100
            'D' -> 1000
            else -> throw Exception()
        }
    }

    fun part1(gameState: String): Int {
        val game = Game(gameState, 0)
        printGame(game)

        var games = listOf(game)

        do {
            // Map
            games = games.flatMap { doRound(it) }

            // Reduce
            games = reduceSolutions(games)

            println("There are ${games.size} possible games")

            // Repeat
        } while (games.size > 1)

        games.forEach { printGame(it) }

        return games[0].energy
    }

    fun part2(gameState: String): Int {
        return -1
    }
}

fun main() {
    println("Part 1: ${Day23().part1(".......BCBCDADA")}")
    println("Part 2: ${Day23().part2(".......BCBCDADA")}")
}