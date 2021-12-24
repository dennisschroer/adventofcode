package nl.dennisschroer.adventofcode.year2021

import java.lang.Exception

class Day23 {
    /*
     *      Map of the game
     * #########################
     * # 0 1 . 2 . 3 . 4 . 5 6 #
     * ##### A # B # C # D #####
     *     # A # B # C # D #
     *     #################
     */
    data class Game(val hallway: String, val roomA: String, val roomB: String, val roomC: String, val roomD: String, val energy: Int) {
        fun isCompleted(): Boolean = hallway == "......." &&
                roomA.filterNot { it == 'A' }.isEmpty() &&
                roomB.filterNot { it == 'B' }.isEmpty() &&
                roomC.filterNot { it == 'C' }.isEmpty() &&
                roomD.filterNot { it == 'D' }.isEmpty()

        fun roomForLetter(letter: Char): String {
            return when (letter) {
                'A' -> roomA
                'B' -> roomB
                'C' -> roomC
                'D' -> roomD
                else -> throw Exception()
            }
        }

        fun roomContainsOnlyFinalPods(letter: Char): Boolean = roomForLetter(letter).filterNot { it == letter }.isEmpty()
        fun finalPodsCount(letter: Char): Int = roomForLetter(letter).takeLastWhile { it == letter }.count()
        fun firstInRoom(letter: Char): Char = roomForLetter(letter).getOrElse(0) { '.' }

        override fun toString(): String {
            val builder = StringBuilder()

            builder.append(hallway.toCharArray().joinToString(" "))
            builder.append("   Energy $energy\n")
            (0 until 4).forEach { i ->
                builder.append("   ")
                builder.append(listOf('A', 'B', 'C', 'D').map { roomForLetter(it).padStart(4, '.')[i] }.joinToString(" "))
                builder.append("\n")
            }

            return builder.toString()
        }
    }

    /**
     * A rule is a possible move from a room to the hallway or vice versa.
     */
    data class Rule(val room: Char, val hallway: Int, val blockers: List<Int>, val distance: Int) {
        /**
         * Is the move from the room to the hallway allowed?
         */
        fun isMoveToHallwayAllowed(game: Game): Boolean {
            // room should not be empty
            return game.firstInRoom(room) != '.' &&
                    // hallway should be empty
                    game.hallway[hallway] == '.' &&
                    // everything in between should be empty
                    blockers.all { game.hallway[it] == '.' } &&
                    // Do not allow "finished" amphipods to move
                    !game.roomContainsOnlyFinalPods(room)
        }

        /**
         * Is the move from the hallway to the room allowed?
         */
        fun isMoveToRoomAllowed(game: Game): Boolean {
            val pod = game.hallway[hallway]

            // Pod can only move to its own room
            return pod == room &&
                    // everything in between should be empty
                    blockers.all { game.hallway[it] == '.' } &&
                    // room should only contain final pods
                    game.roomContainsOnlyFinalPods(room)
        }
    }

    val rules = listOf(
        Rule('A', 0, listOf(1), 3),
        Rule('A', 1, listOf(), 2),
        Rule('A', 2, listOf(), 2),
        Rule('A', 3, listOf(2), 4),
        Rule('A', 4, listOf(2, 3), 6),
        Rule('A', 5, listOf(2, 3, 4), 8),
        Rule('A', 6, listOf(2, 3, 4, 5), 9),

        Rule('B', 0, listOf(1, 2), 5),
        Rule('B', 1, listOf(2), 4),
        Rule('B', 2, listOf(), 2),
        Rule('B', 3, listOf(), 2),
        Rule('B', 4, listOf(3), 4),
        Rule('B', 5, listOf(3, 4), 6),
        Rule('B', 6, listOf(3, 4, 5), 7),

        Rule('C', 0, listOf(1, 2, 3), 7),
        Rule('C', 1, listOf(2, 3), 6),
        Rule('C', 2, listOf(3), 4),
        Rule('C', 3, listOf(), 2),
        Rule('C', 4, listOf(), 2),
        Rule('C', 5, listOf(4), 4),
        Rule('C', 6, listOf(4, 5), 5),

        Rule('D', 0, listOf(1, 2, 3, 4), 9),
        Rule('D', 1, listOf(2, 3, 4), 8),
        Rule('D', 2, listOf(3, 4), 6),
        Rule('D', 3, listOf(4), 4),
        Rule('D', 4, listOf(), 2),
        Rule('D', 5, listOf(), 2),
        Rule('D', 6, listOf(5), 3),
    )

    /**
     * Play a single round executing all possible rules and returning all possible next game states.
     */
    private fun doRound(game: Game): List<Game> {
        // If it is completed keep it as a solution
        if (game.isCompleted()) return listOf(game)

        // Optimalization: if one amphipod can be moved in the room, always prefer to do this first
        val moveToRoom = rules.find { it.isMoveToRoomAllowed(game) }
        if (moveToRoom != null) {
            return listOf(applyMoveToRoom(game, moveToRoom))
        }

        // Apply all possible rules
        return rules.filter { it.isMoveToHallwayAllowed(game) }.map { rule -> applyMoveToHallway(game, rule) }
    }

    /**
     * Return a new game where a pod is moved from a room to a position in the hallway
     */
    private fun applyMoveToHallway(game: Game, rule: Rule): Game {
        val pod = game.firstInRoom(rule.room)

        return Game(
            game.hallway.toMutableList().also { it[rule.hallway] = pod }.joinToString(""),
            if (rule.room == 'A') game.roomA.drop(1) else game.roomA,
            if (rule.room == 'B') game.roomB.drop(1) else game.roomB,
            if (rule.room == 'C') game.roomC.drop(1) else game.roomC,
            if (rule.room == 'D') game.roomD.drop(1) else game.roomD,
            game.energy + (scoreForLetter(pod) * rule.distance)
        )
    }

    /**
     * Return a new game where a pod in the hallway is moved to its room
     */
    private fun applyMoveToRoom(game: Game, rule: Rule): Game {
        val pod = game.hallway[rule.hallway]

        return Game(
            game.hallway.toMutableList().also { it[rule.hallway] = '.' }.joinToString(""),
            if (rule.room == 'A') pod + game.roomA else game.roomA,
            if (rule.room == 'B') pod + game.roomB else game.roomB,
            if (rule.room == 'C') pod + game.roomC else game.roomC,
            if (rule.room == 'D') pod + game.roomD else game.roomD,
            game.energy + (scoreForLetter(pod) * rule.distance)
        )
    }

    private fun reduceSolutions(games: List<Game>): List<Game> {
        if (games.isEmpty()) return games

        // Some possible optimization here: all games seem to finish in the same round
        val (completedGames, uncompletedGames) = games.partition { it.isCompleted() }

        val bestCompletedGame = completedGames.minByOrNull { it.energy }

        // If an uncompleted game already uses more energy don't bother
        val bestUncompletedGames = uncompletedGames.filter { it.energy < (bestCompletedGame?.energy ?: Int.MAX_VALUE) }

        println("${games.size} games contain ${completedGames.size} completed games and ${uncompletedGames.size} uncompleted games (reduced to ${bestUncompletedGames.size})")

        // Keep only the best completed solution plus all promising uncompleted games
        if (bestCompletedGame != null) {
            return listOf(bestCompletedGame) + bestUncompletedGames
        }
        return bestUncompletedGames
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

    private fun applyFixedEnergy(game: Game): Game {
        // Energy required to move to the front of the room.
        val moveOutEnergy = listOf('A', 'B', 'C', 'D').flatMap { room ->
            game.roomForLetter(room).dropLastWhile { it == room }.mapIndexed { index, char -> index * scoreForLetter(char) }
        }.sum()

        // Energy required to fill the room from the front of the room
        val moveInEnergy = listOf('A', 'B', 'C', 'D').flatMap { room ->
            game.roomForLetter(room).dropLastWhile { it == room }.mapIndexed { index, _ -> index * scoreForLetter(room) }
        }.sum()

        return Game(game.hallway, game.roomA, game.roomB, game.roomC, game.roomD, moveOutEnergy + moveInEnergy)
    }

    fun runGame(initialGame: Game): Game {
        val game = applyFixedEnergy(initialGame)
        println(game)

        var games = listOf(game)

        do {
            // Map
            games = games.flatMap { doRound(it) }

            // Reduce
            games = reduceSolutions(games)

            // Repeat
        } while (games.size > 1)

        games.forEach { println(it) }

        return games[0]
    }

    fun part1(initialGame: Game): Int {
        return runGame(initialGame).energy
    }

    fun part2(initialGame: Game): Int {
        return runGame(initialGame).energy
    }
}

fun main() {
    println("Part 1: ${Day23().part1(Day23.Game(".......", "BC", "BC", "DA", "DA", 0))}")
    println("Part 2: ${Day23().part2(Day23.Game(".......", "BDDC", "BCBC", "DBAA", "DACA", 0))}")
}