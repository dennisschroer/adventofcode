package nl.dennisschroer.adventofcode.year2021

class Day9 {
    data class Dot(val x: Int, val y: Int)

    fun part1(input: List<String>): Int {
        val heightMap = parseInput(input)
        val lowPoints = findLowPoints(heightMap)

        return lowPoints.sumOf { it.second } + lowPoints.size
    }

    private fun findLowPoints(heightMap: Map<Dot, Int>): List<Pair<Dot, Int>> {
        val lowPoints = mutableListOf<Pair<Dot, Int>>()
        heightMap.forEach { (location, height) ->
            if (height < listOfNotNull(
                    heightMap[Dot(location.x, location.y - 1)],
                    heightMap[Dot(location.x, location.y + 1)],
                    heightMap[Dot(location.x - 1, location.y)],
                    heightMap[Dot(location.x + 1, location.y)]
                ).minOf { it }
            ) {
                lowPoints += location to height
            }
        }

        return lowPoints
    }

    fun part2(input: List<String>): Int {
        val heightMap = parseInput(input)
        val lowPoints = findLowPoints(heightMap)
        val basins = lowPoints.map { lowPoint -> findBasin(lowPoint.first, heightMap) }.sortedByDescending { it.size }
        return basins.take(3).map { it.size }.reduce(Int::times)
    }

    private fun findBasin(location: Dot, heightMap: Map<Dot, Int>): Set<Pair<Dot, Int>> {
        val height = heightMap[location]!!

        val basin = mutableSetOf(location to height)

        val north = Dot(location.x, location.y - 1)
        val south = Dot(location.x, location.y + 1)
        val west = Dot(location.x - 1, location.y)
        val east = Dot(location.x + 1, location.y)

        heightMap[north]?.let { if (it != 9 && it > height) basin.addAll(findBasin(north, heightMap)) }
        heightMap[south]?.let { if (it != 9 && it > height) basin.addAll(findBasin(south, heightMap)) }
        heightMap[west]?.let { if (it != 9 && it > height) basin.addAll(findBasin(west, heightMap)) }
        heightMap[east]?.let { if (it != 9 && it > height) basin.addAll(findBasin(east, heightMap)) }

        return basin
    }

    fun parseInput(input: List<String>): Map<Dot, Int> {
        return input.mapIndexed { y, row -> row.mapIndexed { x, height -> Dot(x, y) to height.digitToInt() } }.flatten().toMap()
    }
}

fun main() {
    println("Part 1: ${Day9().part1(readLines("day9"))}")
    println("Part 2: ${Day9().part2(readLines("day9"))}")
}