package nl.dennisschroer.adventofcode.year2021

import java.lang.Exception

class Day24 {
    data class Either(val first: Char?, val second: Int?) {
        constructor(first: Char?) : this(first, null)
        constructor(second: Int) : this(null, second)
    }

    data class Command(val instruction: String, val a: Char, val b: Either?)
    data class State(val memory: MutableMap<Char, Int> = mutableMapOf('w' to 0, 'x' to 0, 'y' to 0, 'z' to 0), val input: List<Int> = listOf())

    fun part1(input: List<String>): Long {
        val commands = parseCommands(input)

        val largest = input().first { runProgram(commands, it).memory['z'] == 0 }

        return largest.joinToString("").toLong()
    }

    fun part2(input: List<String>): Int {
        return -1
    }

    fun input() = sequence {
        var value = 99999999999999L

        while (value > 0) {
            yield(value.toString().toCharArray().map { it.digitToInt() })

            do {
                value--
            } while (value.toString().contains('0'))
        }
    }

    fun parseCommands(input: List<String>): List<Command> {
        return input.map { line ->
            val parts = line.split(" ")
            Command(
                parts[0],
                parts[1].first(),
                parts.getOrNull(2)?.toIntOrNull().let { if (it != null) Either(it) else Either(parts.getOrNull(2)?.first()) })
        }
    }

    fun runProgram(commands: List<Command>, input: List<Int>): State = commands.fold(State(input = input), this::runCommand)

    fun runCommand(state: State, command: Command): State {
        return when (command.instruction) {
            "inp" -> State(
                state.memory.also { it[command.a] = state.input.first() },
                state.input.drop(1)
            )
            "add" -> State(
                state.memory.also { it[command.a] = it[command.a]!! + getIntVal(it, command.b!!) },
                state.input
            )
            "mul" -> State(
                state.memory.also { it[command.a] = it[command.a]!! * getIntVal(it, command.b!!) },
                state.input
            )
            "div" -> State(
                state.memory.also { it[command.a] = it[command.a]!! / getIntVal(it, command.b!!) },
                state.input
            )
            "mod" -> State(
                state.memory.also { it[command.a] = it[command.a]!! % getIntVal(it, command.b!!) },
                state.input
            )
            "eql" -> State(
                state.memory.also { it[command.a] = if (it[command.a]!! == getIntVal(it, command.b!!)) 1 else 0 },
                state.input
            )
            else -> throw Exception()
        }
    }

    private fun getIntVal(it: MutableMap<Char, Int>, either: Either): Int = if (either.first != null) it[either.first]!! else either.second!!
}

fun main() {
    println("Part 1: ${Day24().part1(readLines("day24"))}")
    println("Part 2: ${Day24().part2(readLines("day24"))}")
}