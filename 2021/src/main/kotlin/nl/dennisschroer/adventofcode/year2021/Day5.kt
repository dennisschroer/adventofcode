package nl.dennisschroer.adventofcode.year2021

import kotlin.math.abs
import kotlin.math.max

class Day5 {
    data class Point(val x: Int, val y: Int)

    fun part1(input: List<String>): Int {
        val lines = toLines(input)
        val overlaps: MutableMap<Pair<Int, Int>, Int> = mutableMapOf()

        lines.forEach { line ->
            handleIfHorizontal(line, overlaps)
            handleIfVertical(line, overlaps)
        }

        return overlaps.filterValues { it > 1 }.count()
    }

    fun part2(input: List<String>): Int {
        val lines = toLines(input)
        val overlaps: MutableMap<Pair<Int, Int>, Int> = mutableMapOf()

        lines.forEach { line ->
            val length = max(abs(line.second.x - line.first.x), abs(line.second.y - line.first.y)) + 1
            val xs = createRange(line.first.x, line.second.x, length)
            val ys = createRange(line.first.y, line.second.y, length)

            assert(xs.size == ys.size)

            (0 until length).forEach { overlaps[xs[it] to ys[it]] = overlaps.getOrDefault(xs[it] to ys[it], 0) + 1 }
        }

        return overlaps.filterValues { it > 1 }.count()
    }

    private fun createRange(a: Int, b: Int, length: Int): List<Int> {
        if (a == b) {
            return List(length) { a }
        }

        return (if (a < b) a..b else a downTo b).toList()
    }

    private fun handleIfVertical(line: Pair<Point, Point>, overlaps: MutableMap<Pair<Int, Int>, Int>) {
        if (line.first.x == line.second.x) { // vertical
            val x = line.first.x
            val ys = listOf(line.first.y, line.second.y).sorted()

            (ys.first()..ys.last()).forEach { y ->
                overlaps[x to y] = overlaps.getOrDefault(x to y, 0) + 1
            }
        }
    }

    private fun handleIfHorizontal(line: Pair<Point, Point>, overlaps: MutableMap<Pair<Int, Int>, Int>) {
        if (line.first.y == line.second.y) { // horizontal
            val y = line.first.y
            val xs = listOf(line.first.x, line.second.x).sorted()

            (xs.first()..xs.last()).forEach { x ->
                overlaps[x to y] = overlaps.getOrDefault(x to y, 0) + 1
            }
        }
    }

    fun toLines(input: List<String>): List<Pair<Point, Point>> {
        return input.map {
            it.split(" -> ").let { toPoint(it[0]) to toPoint(it[1]) }
        }
    }

    fun toPoint(input: String): Point = input.split(",").let { Point(it[0].toInt(), it[1].toInt()) }
}

fun main() {
    println("Part 1: ${Day5().part1(readLines("day5"))}")
    println("Part 2: ${Day5().part2(readLines("day5"))}")
}