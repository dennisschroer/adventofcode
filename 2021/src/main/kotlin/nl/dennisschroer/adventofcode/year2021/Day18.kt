package nl.dennisschroer.adventofcode.year2021

class Day18 {
    data class E(val type: Char, val value: Int) {
        constructor(type: Char) : this(type, 0)
        constructor(value: Int) : this('L', value)

        override fun toString(): String = if (type == 'L') "$value" else type.toString()
    }

    fun part1(input: List<String>): Int {
        return magnitude(addAndReduceAll(input.map { parseToFish(it) }))
    }

    fun part2(input: List<String>): Int {
        val fishes = input.map { parseToFish(it) }

        val pairs = fishes.flatMap { fish -> fishes.map { fish to it } }

        return pairs.maxOf { (a, b) -> magnitude(addAndReduce(a, b)) }
    }

    fun addAndReduceAll(fishes: List<List<E>>): List<E> {
        return fishes.reduce(this::addAndReduce)
    }

    fun addAndReduce(fish1: List<E>, fish2: List<E>): List<E> {
        return reduce(listOf(E('[')) + fish1 + fish2 + listOf(E(']')))
    }

    fun reduce(fish: List<E>): List<E> {
        var currentFish = fish
        var previousFish: List<E>

        do {
            previousFish = currentFish
            currentFish = explodeOrSplit(currentFish)
        } while (currentFish != previousFish)

        return currentFish
    }

    fun explodeOrSplit(fish: List<E>): List<E> {
        println(fishToString(fish))

        val indexToExplode = fish.indices.firstOrNull { index ->
            val head = fish.take(index)
            fish[index].type == '[' && (head.count { it.type == '[' } - head.count { it.type == ']' }) >= 4
        }

        if (indexToExplode != null) {
            println("Exploding ${fishToString(fish.drop(indexToExplode).take(4))}")
            val indexOfFirst = indexToExplode + 1
            val indexOfSecond = indexToExplode + 2
            val indexOfLeft = fish.take(indexToExplode).indexOfLast { it.type == 'L' }
            val indexOfRight = fish.drop(indexToExplode + 3).indexOfFirst { it.type == 'L' } + indexToExplode + 3

            val result = fish.toMutableList()
            if (indexOfLeft >= 0) {
                result[indexOfLeft] = E(result[indexOfLeft].value + result[indexOfFirst].value)
            }
            if (indexOfRight >= 0) {
                result[indexOfRight] = E(result[indexOfRight].value + result[indexOfSecond].value)
            }

            return result.take(indexToExplode) + listOf(E(0)) + result.drop(indexToExplode + 4)
        }

        val indexToSplit = fish.indices.firstOrNull { fish[it].value > 9 }
        if (indexToSplit != null) {
            val value = fish[indexToSplit].value
            println("Splitting $value")
            return fish.take(indexToSplit) + listOf(E('['), E(value / 2), E(value - value / 2), E(']')) + fish.drop(indexToSplit + 1)
        }

        return fish
    }

    fun fishToString(fish: List<E>): String = fish.joinToString(",")
        .replace("[,", "[")
        .replace(",]", "]")

    fun magnitude(fish: List<E>): Int {
        var reducedFish = fish
        while (reducedFish[0].type != 'L') {
            reducedFish = replacePairWithMagnitude(reducedFish)
        }

        return reducedFish[0].value
    }

    private fun replacePairWithMagnitude(fish: List<E>): List<E> {
        var firstPair = -1
        var index = 0
        fish.windowed(4).forEach {
            if (it[0].type == '[' && it[1].type == 'L' && it[2].type == 'L' && it[3].type == ']') {
                firstPair = index
            }
            index++
        }

        return fish.take(firstPair) + E(3 * fish[firstPair + 1].value + 2 * fish[firstPair + 2].value) + fish.drop(firstPair + 4)
    }

    fun parseToFish(line: String): List<E> = line.toList().map { if (it.isDigit()) E(it.digitToInt()) else E(it) }.filter { it.type != ',' }
}

fun main() {
    println("Part 1: ${Day18().part1(readLines("day18"))}")
    println("Part 2: ${Day18().part2(readLines("day18"))}")
}