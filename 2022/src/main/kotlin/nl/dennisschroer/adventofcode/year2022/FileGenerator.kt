package nl.dennisschroer.adventofcode.year2022

import java.io.File

fun main() {
    val year = 2022

    (1..25).forEach {
        File("$year/src/main/kotlin/nl/dennisschroer/adventofcode/year$year/Day$it.kt").writeText("""
            package nl.dennisschroer.adventofcode.year$year

            fun day${it}part1(input: List<String>): Int {
                return -1
            }

            fun day${it}part2(input: List<String>): Int {
                return -1
            }

            fun main() {
                println("Part 1: ${"$"}{day${it}part1(readLines("day$it"))}")
                println("Part 2: ${"$"}{day${it}part2(readLines("day$it"))}")
            }
        """.trimIndent())

        File("$year/src/main/resources/day$it.txt").createNewFile()

        File("$year/src/test/kotlin/nl/dennisschroer/adventofcode/year$year/Day${it}Test.kt").writeText("""
            package nl.dennisschroer.adventofcode.year$year

            import org.junit.jupiter.api.Assertions.assertEquals
            import org.junit.jupiter.api.Test

            internal class Day${it}Test {

                @Test
                fun testPart1() {
                    val lines = readLines("day$it")
                    assertEquals(0, day${it}part1(lines))
                }

                @Test
                fun testPart2() {
                    val lines = readLines("day$it")
                    assertEquals(0, day${it}part2(lines))
                }
            }
        """.trimIndent())

        File("$year/src/test/resources/day${it}.txt").createNewFile()
    }
}