package nl.dennisschroer.adventofcode.year2021

import java.io.File

fun main() {
    (5..25).forEach {
        File("2021/src/main/kotlin/nl/dennisschroer/adventofcode/year2021/Day$it.kt").writeText("""
            package nl.dennisschroer.adventofcode.year2021

            class Day$it {
                fun part1(input: List<String>): Int {
                    return -1
                }

                fun part2(input: List<String>): Int {
                    return -1
                }
            }

            fun main() {
                println("Part 1: ${"$"}{Day$it().part1(readLines("day$it"))}")
                println("Part 2: ${"$"}{Day$it().part2(readLines("day$it"))}")
            }
        """.trimIndent())

        File("2021/src/main/resources/day$it.txt").createNewFile()

        File("2021/src/test/kotlin/nl/dennisschroer/adventofcode/year2021/Day${it}Test.kt").writeText("""
            package nl.dennisschroer.adventofcode.year2021

            import org.junit.jupiter.api.Assertions.assertEquals
            import org.junit.jupiter.api.Test

            internal class Day${it}Test {

                @Test
                fun testPart1() {
                    val lines = readLines("testDay$it")
                    assertEquals(0, Day${it}().part1(lines))
                }

                @Test
                fun testPart2() {
                    val lines = readLines("testDay$it")
                    assertEquals(0, Day${it}().part2(lines))
                }
            }
        """.trimIndent())

        File("2021/src/test/resources/testDay${it}.txt").createNewFile()
    }
}