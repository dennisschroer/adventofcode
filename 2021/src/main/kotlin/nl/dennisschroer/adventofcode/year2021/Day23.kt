package nl.dennisschroer.adventofcode.year2021

class Day23 {
    /*
     *      Map of the game
     * #########################
     * # 0 1 . 2 . 3 . 4 . 5 6 #
     * ##### 7 # 9 # 11# 13#####
     *     # 8 # 10# 12# 14#
     *     #################
     */

    data class Rule(val room: Int, val hallway: Int, val blockers: List<Int>) {
        /**
         * Is the move from the room to the hallway allowed?
         */
        fun isAllowed(game: List<Char>): Boolean {
            // room should not be empty
            return game[room] != '.' &&
                    // hallway should be empty
                    game[hallway] == '.' &&
                    // everything in between should be empty
                    blockers.all { game[it] == '.' } &&
                    // Do not allow "finished" amphipods to move
                    when (room) {
                        7 -> game[7] != 'A' || game[8] != 'A'
                        8 -> game[8] != 'A'
                        9 -> game[9] != 'B' || game[10] != 'B'
                        10 -> game[10] != 'B'
                        11 -> game[11] != 'C' || game[12] != 'C'
                        12 -> game[12] != 'C'
                        13 -> game[13] != 'D' || game[14] != 'D'
                        14 -> game[14] != 'D'
                        else -> true
                    }
        }

        /**
         * Is the move from the hallway to the room allowed?
         */
        fun isinverseAllowed(game: List<Char>): Boolean {
            return game[room] == '.' && blockers.all { game[it] == '.' } &&
                    when (room) {
                        7 -> game[hallway] == 'A' && game[8] == 'A'
                        8 -> game[hallway] == 'A'
                        9 -> game[hallway] == 'B' && game[10] == 'B'
                        10 -> game[hallway] == 'B'
                        11 -> game[hallway] == 'C' && game[12] == 'C'
                        12 -> game[hallway] == 'C'
                        13 -> game[hallway] == 'D' && game[14] == 'D'
                        14 -> game[hallway] == 'D'
                        else -> false
                    }
        }
    }

    val rules = listOf(
        Rule(7, 0, listOf(1)),
        Rule(7, 1, listOf()),
        Rule(7, 2, listOf()),
        Rule(7, 3, listOf(2)),
        Rule(7, 4, listOf(2, 3)),
        Rule(7, 5, listOf(2, 3, 4)),
        Rule(7, 6, listOf(2, 3, 4, 5)),

        Rule(8, 0, listOf(7, 1)),
        Rule(8, 1, listOf(7)),
        Rule(8, 2, listOf(7)),
        Rule(8, 3, listOf(7, 2)),
        Rule(8, 4, listOf(7, 2, 3)),
        Rule(8, 5, listOf(7, 2, 3, 4)),
        Rule(8, 6, listOf(7, 2, 3, 4, 5)),

        Rule(9, 0, listOf(1, 2)),
        Rule(9, 1, listOf(2)),
        Rule(9, 2, listOf()),
        Rule(9, 3, listOf()),
        Rule(9, 4, listOf(3)),
        Rule(9, 5, listOf(3, 4)),
        Rule(9, 6, listOf(3, 4, 5)),

        Rule(10, 0, listOf(9, 1, 2)),
        Rule(10, 1, listOf(9, 2)),
        Rule(10, 2, listOf(9)),
        Rule(10, 3, listOf(9)),
        Rule(10, 4, listOf(9, 3)),
        Rule(10, 5, listOf(9, 3, 4)),
        Rule(10, 6, listOf(9, 3, 4, 5)),

        Rule(11, 0, listOf(1, 2, 3)),
        Rule(11, 1, listOf(2, 3)),
        Rule(11, 2, listOf(3)),
        Rule(11, 3, listOf()),
        Rule(11, 4, listOf()),
        Rule(11, 5, listOf(4)),
        Rule(11, 6, listOf(4, 5)),

        Rule(12, 0, listOf(11, 1, 2, 3)),
        Rule(12, 1, listOf(11, 2, 3)),
        Rule(12, 2, listOf(11, 3)),
        Rule(12, 3, listOf(11)),
        Rule(12, 4, listOf(11)),
        Rule(12, 5, listOf(11, 4)),
        Rule(12, 6, listOf(11, 4, 5)),

        Rule(13, 0, listOf(1, 2, 3, 4)),
        Rule(13, 1, listOf(2, 3, 4)),
        Rule(13, 2, listOf(3, 4)),
        Rule(13, 3, listOf(4)),
        Rule(13, 4, listOf()),
        Rule(13, 5, listOf()),
        Rule(13, 6, listOf(5)),

        Rule(14, 0, listOf(13, 1, 2, 3, 4)),
        Rule(14, 1, listOf(13, 2, 3, 4)),
        Rule(14, 2, listOf(13, 3, 4)),
        Rule(14, 3, listOf(13, 4)),
        Rule(14, 4, listOf(13)),
        Rule(14, 5, listOf(13)),
        Rule(14, 6, listOf(13, 5)),
    )

    fun printGame(game: List<Char>) {
        println(game.take(7).joinToString(" "))
        print("   ")
        println(listOf(7, 9, 11, 13).map { game[it] }.joinToString(" "))
        print("   ")
        println(listOf(8, 10, 12, 14).map { game[it] }.joinToString(" "))
    }

    private fun reduceSolutions(games: List<List<Char>>): List<List<Char>> {
        val scores = games.associateWith { completionScore(it) }
        val maxScore = scores.maxOf { it.value }
        return scores.filter { it.value == maxScore }.keys.toList()
    }

    private fun completionScore(game: List<Char>): Int {
        return listOf(
            game[7] == 'A' && game[8] == 'A',
            game[8] == 'A',
            game[9] == 'A' && game[10] == 'A',
            game[10] == 'A',
            game[11] == 'A' && game[12] == 'A',
            game[12] == 'A',
            game[13] == 'A' && game[14] == 'A',
            game[14] == 'A',
        ).count { it }
    }

    /**
     * Play a single round executing all possible rules and returning all possible next game states.
     */
    private fun doRound(game: List<Char>): List<List<Char>> {
        val allowedRules = rules.filter { it.isinverseAllowed(game) }
        return if (allowedRules.isNotEmpty()) {
            // Optimalization: if one amphipod can be moved in the room, always prefer to do this first
            return listOf(applyInverseRule(game, allowedRules[0]))
        } else {
            rules.filter { it.isAllowed(game) }.map { rule -> applyRule(game, rule) }
        }
    }

    private fun applyRule(game: List<Char>, rule: Rule): List<Char> {
        return game.toMutableList().also {
            it[rule.hallway] = game[rule.room]
            it[rule.room] = '.'
        }
    }

    private fun applyInverseRule(game: List<Char>, rule: Rule): List<Char> {
        return game.toMutableList().also {
            it[rule.room] = game[rule.hallway]
            it[rule.hallway] = '.'
        }
    }

    fun part1(game: List<Char>): Int {
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

        return -1
    }

    fun part2(game: List<Char>): Int {
        return -1
    }
}

fun main() {
    println("Part 1: ${Day23().part1(".......BCBCDADA".toList())}")
    println("Part 2: ${Day23().part2(".......BCBCDADA".toList())}")
}