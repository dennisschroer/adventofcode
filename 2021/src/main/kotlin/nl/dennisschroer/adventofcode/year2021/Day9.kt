package nl.dennisschroer.adventofcode.year2021

class Day9 {
    fun part1(input: List<String>): Int {
        val heightMap = input.map { it.asIterable().map { c -> c.digitToInt() } }

        val lowPoints = mutableListOf<Int>()
        (heightMap[0].indices).forEach { x ->
            (heightMap.indices).forEach { y ->
                val height: Int = heightMap[y][x]
                if (height < listOfNotNull(
                        heightMap.getOrNull(y - 1)?.getOrNull(x),
                        heightMap.getOrNull(y + 1)?.getOrNull(x),
                        heightMap.getOrNull(y)?.getOrNull(x - 1),
                        heightMap.getOrNull(y)?.getOrNull(x + 1)
                    ).minOf { it }
                ) {
                    lowPoints += height
                }
            }
        }

        return lowPoints.sum() + lowPoints.size
    }

    fun part2(input: List<String>): Int {
        return -1
    }
}

fun main() {
    println("Part 1: ${Day9().part1(readLines("day9"))}")
    println("Part 2: ${Day9().part2(readLines("day9"))}")
}