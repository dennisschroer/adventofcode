package nl.dennisschroer.adventofcode.year2021

import java.lang.Exception

class Day13 {
    data class Dot(val x: Int, val y: Int)

    fun part1(input: String): Int {
        val (dots, instructions) = parseInput(input)
        return foldPaper(dots, instructions.first()).size
    }

    private fun foldPaper(dots: Set<Dot>, instruction: Pair<Char, Int>): Set<Dot> {
        return when (instruction.first) {
            'y' -> dots.fold(setOf()) { result, dot -> result + Dot(dot.x, if (dot.y > instruction.second) 2 * instruction.second - dot.y else dot.y) }
            'x' -> dots.fold(setOf()) { result, dot -> result + Dot(if (dot.x > instruction.second) 2 * instruction.second - dot.x else dot.x, dot.y) }
            else -> throw Exception()
        }
    }

    fun part2(input: String) {
        val (dots, instructions) = parseInput(input)
        val result = instructions.fold(dots) { dots2, instruction -> foldPaper(dots2, instruction) }

        (0..result.maxOf { it.y }).forEach { y ->
            (0..result.maxOf { it.x }).forEach { x ->
                print(if (result.contains(Dot(x,y))) '█' else ' ')
            }
            println()
        }
    }

    fun parseInput(input: String): Pair<Set<Dot>, List<Pair<Char, Int>>> {
        val dots = input.substringBefore("\n\n").split("\n").map { line -> line.split(",").let { Dot(it[0].toInt(), it[1].toInt()) } }.toSet()
        val instructions = input.substringAfter("\n\n").split("\n").map { line -> line.substring(11).split("=").let { it[0][0] to it[1].toInt() } }

        return dots to instructions
    }
}

fun main() {
    println("Part 1: ${Day13().part1(readFile("day13"))}")
    println("Part 2:")
    Day13().part2(readFile("day13"))
}