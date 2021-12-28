package nl.dennisschroer.adventofcode.year2021

class Day20 {
    fun part1(input: String): Int {
        val (algorithm, image) = parseInput(input)

        printImage(image)
        val result = removeBorder((0 until 2).fold(addBorder(image, 100)) { i, _ -> enhance(i, algorithm) }, 98)
        printImage(result)

        return result.sumOf { it.count { pixel -> pixel == '#' } }
    }

    fun part2(input: String): Int {
        val (algorithm, image) = parseInput(input)

        printImage(image)
        val result = removeBorder((0 until 50).fold(addBorder(image, 100)) { i, _ -> enhance(i, algorithm) }, 50)
        printImage(result)

        return result.sumOf { it.count { pixel -> pixel == '#' } }
    }

    private fun parseInput(input: String): Pair<String, List<List<Char>>> {
        val algorithm = input.substringBefore("\n\n")
        val image = input.substringAfter("\n\n").split("\n").map { it.toList() }

        assert(algorithm.length == 512) { "Length of algorithm should be 512, is ${algorithm.length}" }

        return algorithm to image
    }

    private fun addBorder(image: List<List<Char>>, borderSize: Int): List<List<Char>> {
        val imageWidth = image[0].size

        val fullLine = (0 until imageWidth + 2 * borderSize).map { '.' }
        val side = (0 until borderSize).map { '.' }

        return (0 until borderSize).map { fullLine } +
                image.map { side + it + side } +
                (0 until borderSize).map { fullLine }
    }

    private fun removeBorder(image: List<List<Char>>, borderSize: Int): List<List<Char>> {
        return image.drop(borderSize).dropLast(borderSize).map { it.drop(borderSize).dropLast(borderSize) }
    }

    private fun enhance(image: List<List<Char>>, algorithm: String): List<List<Char>> {
        return image.indices.map { y ->
            image[0].indices.map { x ->
                val word = (-1..1).flatMap { dy -> (-1..1).map { dx -> pixelAt(image, x + dx, y + dy) } }.joinToString("")
                val number = word.replace('.', '0').replace('#', '1').toInt(2)
                algorithm[number]
            }
        }
    }

    private fun pixelAt(image: List<List<Char>>, x: Int, y: Int): Char {
        if (image.indices.contains(y) && image[y].indices.contains(x)) {
            return image[y][x]
        }
        return '.'
    }

    private fun printImage(image: List<List<Char>>) {
        image.forEach { println(it.joinToString("")) }
        println()
    }
}

fun main() {
    println("Part 1: ${Day20().part1(readFile("day20"))}")
    println("Part 2: ${Day20().part2(readFile("day20"))}")
}