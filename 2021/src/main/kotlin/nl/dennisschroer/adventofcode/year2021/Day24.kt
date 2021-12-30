package nl.dennisschroer.adventofcode.year2021

import kotlinx.coroutines.async
import kotlinx.coroutines.awaitAll
import kotlinx.coroutines.coroutineScope
import kotlinx.coroutines.launch
import kotlinx.coroutines.runBlocking
import java.lang.Exception

class Day24 {
    data class Either(val first: Char?, val second: Int?) {
        constructor(first: Char?) : this(first, null)
        constructor(second: Int) : this(null, second)
    }

    data class Command(val instruction: String, val a: Char, val b: Either?) {
        override fun toString(): String = "$instruction $a ${b?.first ?: ""}${b?.second ?: ""}"
    }

    data class Memory(val w: Int = 0, val x: Int = 0, val y: Int = 0, val z: Int = 0)
    data class State(val memory: Map<Char, Int> = mutableMapOf('w' to 0, 'x' to 0, 'y' to 0, 'z' to 0), val input: List<Int> = listOf())

    fun part1(input: List<String>): Long {
        val commands = parseCommands(input)

        // Smart execution: first statements are always equal for same input
        var remainingCommands = optimize(commands)
        val blocks = (0 until 14).associateWith {
            val index = remainingCommands.drop(1).indexOfFirst { it.instruction == "inp" }
            val result = remainingCommands.take(index + 1)
            remainingCommands = remainingCommands.drop(index + 1)
            result
        }

        return runBlocking { findLargest(blocks.values, State())!! }


//        val largest = input().first { runProgram(commands, it).memory['z'] == 0 }

//        return largest.joinToString("").toLong()
//        return -1
    }

    private suspend fun findLargest(blocks: Collection<List<Command>>, state: State): Long? {
        if (blocks.isEmpty()) {
            return null
        }

        val largest = coroutineScope {
            listOf(9, 8, 7, 6, 5, 4, 3, 2, 1).associateWith { digit ->
                if (blocks.size > 10) {
                    repeat(14 - blocks.size) { print("> ") }
                    println(digit)
                }

                val nextState = runProgram(blocks.first(), State(memory = state.memory, input = listOf(digit)))
                async { findLargest(blocks.drop(1), nextState) }
            }.mapValues { awaitAll(it.value) }.toList().firstOrNull { it.second != null }
        }

        if (largest != null) {
            return "${largest.first}${largest.second}".toLong()
        }

        return null
    }

    private fun custom(state: State, a: Int, b: Int) {
        var w = state.memory['w']!!
        var x = state.memory['x']!!
        var y = state.memory['y']!!
        var z = state.memory['z']!!

        x = if (z % 26 + a == w) 1 else 0
        y = 25 * x + 1
        z = z * y
        y = (w + b) * x
        z = z + y


        if (z % 26 + a == w) {
            z = 26 * z + w + b
        }
    }

    private fun optimize(commands: List<Command>): List<Command> {
        val result = mutableListOf<Command>()

        var index = 0
        var command: Command
        var nextCommand: Command?

        while (index < commands.size) {
            command = commands[index]
            nextCommand = commands.getOrNull(index + 1)

            if (command.instruction == "mul" && command.b?.second == 0) {
                // Dont bother multiplying just reset
                result.add(Command("res", command.a, null))
            } else if (command.instruction == "eql" && nextCommand?.instruction == "eql" && command.a == nextCommand.a && nextCommand.b?.second == 0) {
                // replace with neq instruction
                result.add(Command("neq", command.a, command.b))
                index++
            } else if (command.instruction == "div" && command.b?.second == 1) {
                // Unnecessary step, just remove
            } else {
                result.add(command)
            }

            index++
        }

        println("Program optimized from ${commands.size} to ${result.size} commands")
        result.forEach { println(it) }

        return result
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

    fun optimizeAndRunProgram(commands: List<Command>, input: List<Int>): State = optimize(commands).fold(State(input = input), this::runCommand)

    fun runProgram(commands: List<Command>, input: List<Int>): State = runProgram(commands, State(input = input))

    fun runProgram(commands: List<Command>, state: State): State = commands.fold(state, this::runCommand)

    fun runCommand(state: State, command: Command): State {
        return when (command.instruction) {
            "inp" -> State(
                state.memory.toMutableMap().also { it[command.a] = state.input.first() },
                state.input.drop(1)
            )
            "add" -> State(
                state.memory.toMutableMap().also { it[command.a] = it[command.a]!! + getIntVal(it, command.b!!) },
                state.input
            )
            "mul" -> State(
                state.memory.toMutableMap().also { it[command.a] = it[command.a]!! * getIntVal(it, command.b!!) },
                state.input
            )
            "div" -> State(
                state.memory.toMutableMap().also { it[command.a] = it[command.a]!! / getIntVal(it, command.b!!) },
                state.input
            )
            "mod" -> State(
                state.memory.toMutableMap().also { it[command.a] = it[command.a]!! % getIntVal(it, command.b!!) },
                state.input
            )
            "eql" -> State(
                state.memory.toMutableMap().also { it[command.a] = if (it[command.a]!! == getIntVal(it, command.b!!)) 1 else 0 },
                state.input
            )
            "neq" -> State(
                state.memory.toMutableMap().also { it[command.a] = if (it[command.a]!! == getIntVal(it, command.b!!)) 0 else 1 },
                state.input
            )
            "res" -> State(
                state.memory.toMutableMap().also { it[command.a] = 0 },
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