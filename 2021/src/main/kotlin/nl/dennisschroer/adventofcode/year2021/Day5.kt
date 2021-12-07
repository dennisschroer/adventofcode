package nl.dennisschroer.adventofcode.year2021

class Day5 {
    data class Point(val x: Int, val y: Int)

    fun part1(input: List<String>): Int {
        val lines = toLines(input)
        val overlaps: MutableMap<Pair<Int, Int>, Int> = mutableMapOf()

        lines.forEach { line ->
            if (line.first.x == line.second.x) { // vertical
                val x = line.first.x
                val ys = listOf(line.first.y, line.second.y).sorted()

                (ys.first()..ys.last()).forEach { y ->
                    overlaps[x to y] = overlaps.getOrDefault(x to y, 0) + 1
                }
            }

            if (line.first.y == line.second.y) { // horizontal
                val y = line.first.y
                val xs = listOf(line.first.x, line.second.x).sorted()

                (xs.first()..xs.last()).forEach { x ->
                    overlaps[x to y] = overlaps.getOrDefault(x to y, 0) + 1
                }
            }
        }

        return overlaps.filterValues { it > 1 }.count()
    }

    fun part2(input: List<String>): Int {
        return -1
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