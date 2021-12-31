package nl.dennisschroer.adventofcode.year2021

import kotlinx.coroutines.async
import kotlinx.coroutines.awaitAll
import kotlinx.coroutines.coroutineScope
import kotlinx.coroutines.launch
import kotlinx.coroutines.runBlocking
import java.lang.Exception
import java.util.SortedSet

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

    // Observations:
    // - 1: a is negative iff c == 26
    // - 2: a > 10 if c == 1, thus x != w
    // - 3: Calculations can be seen as if in base 26
    //
    // x = z % 26 + a
    // z = z / c
    // if (x != input) {
    //    z = (26 * z) + input + b
    // }
    // return z
    //              0   1   2   3   4   5    6   7   8   9  10  11  12  13
    val a = listOf(11, 14, 10, 14, -8, 14, -11, 10, -6, -9, 12, -5, -4, -9)
    val b = listOf(7,  8,  16,  8,  3, 12,   1,  8,  8, 14,  4, 14, 15,  6)
    val c = listOf(1,  1,   1,  1, 26,  1,  26,  1, 26, 26,  1, 26, 26, 26)

    // Conclusions:
    // w4 == w3                    w3=9 w4=9         w3=1 w4=1
    // w6 == w5 + 1 mod 26         w5=8 w6=9         w5=1 w6=2
    // w8 == w7 + 2 mod 26         w7=7 w8=9         w7=1 w8=3
    // w9 == w2 + 7 mod 26         w2=2 w9=9         w2=1 w9=8
    // w11 == w10 - 1 mod 26       w10=9 w11=8       w10=2 w11=1
    // w12 == w1 + 4 mod 26        w1=5 w12=9        w1=1 w12=5
    // w13 == w0 - 2 mod 26        w0=9 w13=7        w0=3 w13=1
    // => largest = 95299897999897
    // => smallest = 31111121382151

    fun part1(input: List<String>): Long {
        val commands = parseCommands(input)

        // Smart execution: first statements are always equal for same input
        var remainingCommands = commands
        val blocks = (0 until 14).map {
            val index = remainingCommands.drop(1).indexOfFirst { it.instruction == "inp" }
            val result = if (index > 0) {
                remainingCommands.take(index + 1)
            } else {
                remainingCommands
            }
            remainingCommands = remainingCommands.drop(index + 1)
            result
        }


        var possibleOutcomes: SortedSet<Int> = sortedSetOf(0)
        val digits = mutableMapOf<Int, Int>()

        println("a = " + blocks.map { it[5].b?.second })
        println("b = " + blocks.map { it[15].b?.second })
        println("c = " + blocks.map { it[4].b?.second })

        blocks.forEachIndexed { index, commands ->
            println(index)

            val possibleZs = possibleOutcomes
            if (a[index] < 10) { // Optimization from observation 2
                possibleOutcomes = listOf(9, 8, 7, 6, 5, 4, 3, 2, 1).flatMap { input ->
                    digits[index] = input
                    possibleZs.map { z ->
                        val outcome = custom(input, z, a[index], b[index], c[index])
                        if (outcome == 0) {
                            println("OUTPUT 0 at index $index for input $input and z $z, digits = ${digits.map { it.value }.joinToString("")}")
                        }
                        outcome
                    }.also { println() }
                }.toSortedSet()
            }
        }

        println(possibleOutcomes.contains(0))

        return -1

        //return findLargest(blocks)

//        val largest = input().first { runProgram(commands, it).memory['z'] == 0 }
//
//        return largest.joinToString("").toLong()
//        return -1
    }

    private fun findLargest(blocks: Collection<List<Command>>): Long {
        listOf(9, 8, 7, 6, 5, 4, 3, 2, 1).forEach {
            println("===== $it =====")
            println(runProgram(blocks.first(), State(input = listOf(it))))
        }

        val digit = listOf(9, 8, 7, 6, 5, 4, 3, 2, 1).first { runProgram(blocks.first(), State(input = listOf(it))).memory['z'] == 0 }.toLong()

        return if (blocks.size == 1) {
            digit
        } else {
            val largest = findLargest(blocks.drop(1))
            "${digit}${largest}".toLong()
        }


//        val largest = coroutineScope {
//            listOf(9, 8, 7, 6, 5, 4, 3, 2, 1).associateWith { digit ->
//                if (blocks.size > 10) {
//                    repeat(14 - blocks.size) { print("> ") }
//                    println(digit)
//                }
//
//                val nextState = runProgram(blocks.first(), State(memory = state.memory, input = listOf(digit)))
//                async { findLargest(blocks.drop(1), nextState) }
//            }.mapValues { awaitAll(it.value) }.toList().firstOrNull { it.second != null }
//        }


    }

//    private fun doCustom() {
//        val a = listOf(11, 14, 10, 14, -8, 14, -11, 10, -6, -9, 12, -5, -4)
//        val b = listOf(7, 8, 16, 8, 3, 12, 1, 8, 8, 14, 4, 14, 15)
//        val c = listOf(1, 1, 1, 1, 26, 1, 26, 1, 26, 26, 1, 26, 26)
//
//        val result = doCustomRecursive(0, a, b, c)
//
//    }
//
//    private fun doCustomRecursive(z: Int, a: List<Int>, b: List<Int>, c: List<Int>): Long {
//        listOf(9, 8, 7, 6, 5, 4, 3, 2, 1).forEach {
//            val result = doCustomRecursive(custom(it, z, a.first(), b.first(), c.first()), a.drop(1), b.drop(1), c.drop(1))
//            return "$it$result".toLong()
//
//        }
//    }

    private fun custom(input: Int, z: Int, a: Int, b: Int, c: Int): Int {
        // a = var2
        // b = var3
        // c = var1
        val x = z % 26 + a
        var z2 = z / c
        if (x != input) {
            z2 = (26 * z2) + input + b
        }
        return z2
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
        val result = when (command.instruction) {
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

        println("$command   \t$state \t=>\t$result")

        return result
    }

    private fun getIntVal(it: MutableMap<Char, Int>, either: Either): Int = if (either.first != null) it[either.first]!! else either.second!!
}

fun main() {
    println("Part 1: ${Day24().part1(readLines("day24"))}")
    println("Part 2: ${Day24().part2(readLines("day24"))}")
}