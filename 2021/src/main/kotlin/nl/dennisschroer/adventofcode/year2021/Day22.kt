package nl.dennisschroer.adventofcode.year2021

import kotlin.math.max
import kotlin.math.min

class Day22 {
    data class RebootStep(val command: String, val xs: IntRange, val ys: IntRange, val zs: IntRange)

    fun part1(input: List<String>): Int {
        val result = sortedSetOf<Int>()
        val validRange = -50..50
        var hash: Int

        parseRebootSteps(input).forEach { step ->
            step.xs.forEach { x ->
                step.ys.forEach { y ->
                    step.zs.forEach { z ->
                        if (validRange.contains(x) && validRange.contains(y) && validRange.contains(z)) {
                            // By multiplying with prime numbers each coordinate gets assigned a unique number
                            hash = x * 10000 + y * 100 + z
                            if (step.command == "on") {
                                result.add(hash)
                            } else {
                                result.remove(hash)
                            }
                        }
                    }
                }
            }
        }

        return result.size
    }

    fun part2(input: List<String>): Int {
        return -1
    }

    fun parseRebootSteps(input: List<String>): List<RebootStep> {
        return input.map { line ->
            val command = line.substringBefore(" ")
            val coordinates = line.substringAfter(" ").split(",").map { it.drop(2) }

            RebootStep(
                command,
                toIntRange(coordinates[0]),
                toIntRange(coordinates[1]),
                toIntRange(coordinates[2])
            )
        }
    }

    private fun toIntRange(rangeDefinition: String): IntRange =
        max(rangeDefinition.substringBefore(".").toInt(), -50)..min(rangeDefinition.substringAfterLast(".").toInt(), 50)
}

fun main() {
    println("Part 1: ${Day22().part1(readLines("day22"))}")
    println("Part 2: ${Day22().part2(readLines("day22"))}")
}