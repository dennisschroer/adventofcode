package nl.dennisschroer.adventofcode.year2021

class Day15 {
    fun part1(input: List<String>): Int {
        val initialMap: List<MutableList<Pair<Int, Int>>> = input.map { it.toList().map { risk -> risk.digitToInt() to 1000000 }.toMutableList() }

        initialMap[0][0] = initialMap[0][0].first to 0

        val result = (0 until initialMap.size*2).fold(initialMap) { map, _ ->
            map.indices.map { y ->
                map[y].mapIndexed { x, value ->
                    value.first to (listOf(
                        value.second,
                        value.first + getCostAt(map, x - 1, y),
                        value.first + getCostAt(map, x + 1, y),
                        value.first + getCostAt(map, x, y - 1),
                        value.first + getCostAt(map, x, y + 1)
                    ).minOf { it })
                }.toMutableList()
            }
        }

        return result[result.size - 1][result[0].size - 1].second
    }

    private fun getCostAt(map: List<List<Pair<Int, Int>>>, x: Int, y: Int): Int {
        if (map.indices.contains(y) && map[0].indices.contains(x)) {
            return map[y][x].second
        }
        return 1000000
    }

    fun part2(input: List<String>): Int {
        return -1
    }
}

fun main() {
    println("Part 1: ${Day15().part1(readLines("day15"))}")
    println("Part 2: ${Day15().part2(readLines("day15"))}")
}