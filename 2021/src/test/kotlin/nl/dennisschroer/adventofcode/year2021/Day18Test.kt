package nl.dennisschroer.adventofcode.year2021

import org.junit.jupiter.api.Assertions.assertEquals
import org.junit.jupiter.api.Assertions.assertTrue
import org.junit.jupiter.api.Test

internal class Day18Test {
    @Test
    fun testExplode() {
        val d = Day18()
        assertLike(d.parseToFish("[[[[0,9],2],3],4]"), d.explodeOrSplit(d.parseToFish("[[[[[9,8],1],2],3],4]")))
        assertLike(d.parseToFish("[7,[6,[5,[7,0]]]]"), d.explodeOrSplit(d.parseToFish("[7,[6,[5,[4,[3,2]]]]]")))
        assertLike(d.parseToFish("[[6,[5,[7,0]]],3]"), d.explodeOrSplit(d.parseToFish("[[6,[5,[4,[3,2]]]],1]")))
        assertLike(d.parseToFish("[[3,[2,[8,0]]],[9,[5,[4,[3,2]]]]]"), d.explodeOrSplit(d.parseToFish("[[3,[2,[1,[7,3]]]],[6,[5,[4,[3,2]]]]]")))
        assertLike(d.parseToFish("[[3,[2,[8,0]]],[9,[5,[7,0]]]]"), d.explodeOrSplit(d.parseToFish("[[3,[2,[8,0]]],[9,[5,[4,[3,2]]]]]")))
    }

    @Test
    fun testReduce() {
        val d = Day18()
        assertLike(d.parseToFish("[[[[0,7],4],[[7,8],[6,0]]],[8,1]]"), d.reduce(d.parseToFish("[[[[[4,3],4],4],[7,[[8,4],9]]],[1,1]]")))
    }

    @Test
    fun testMagnitude() {
        val d = Day18()
        assertEquals(29, d.magnitude(d.parseToFish("[9,1]")))
        assertEquals(21, d.magnitude(d.parseToFish("[1,9]")))
        assertEquals(143, d.magnitude(d.parseToFish("[[1,2],[[3,4],5]]")))
        assertEquals(3488, d.magnitude(d.parseToFish("[[[[8,7],[7,7]],[[8,6],[7,7]]],[[[0,7],[6,6]],[8,7]]]")))
    }

    @Test
    fun testAddAndReduce() {
        val d = Day18()
        assertLike(
            d.parseToFish("[[[[0,7],4],[[7,8],[6,0]]],[8,1]]"),
            d.addAndReduce(
                d.parseToFish("[[[[4,3],4],4],[7,[[8,4],9]]]"),
                d.parseToFish("[1,1]")
            )
        )

        assertLike(
            d.parseToFish("[[[[4,0],[5,4]],[[7,7],[6,0]]],[[8,[7,7]],[[7,9],[5,0]]]]"),
            d.addAndReduce(
                d.parseToFish("[[[0,[4,5]],[0,0]],[[[4,5],[2,6]],[9,5]]]"),
                d.parseToFish("[7,[[[3,7],[4,3]],[[6,3],[8,8]]]]")
            )
        )

        assertLike(
            d.parseToFish("[[[[7,0],[7,7]],[[7,7],[7,8]]],[[[7,7],[8,8]],[[7,7],[8,7]]]]"),
            d.addAndReduce(
                d.parseToFish("[[[[6,7],[6,7]],[[7,7],[0,7]]],[[[8,7],[7,7]],[[8,8],[8,0]]]]"),
                d.parseToFish("[[[[2,4],7],[6,[0,5]]],[[[6,8],[2,8]],[[2,1],[4,5]]]]")
            )
        )

        assertLike(
            d.parseToFish("[[[[7,7],[7,8]],[[9,5],[8,7]]],[[[6,8],[0,8]],[[9,9],[9,0]]]]"),
            d.addAndReduce(
                d.parseToFish("[[[[7,0],[7,7]],[[7,7],[7,8]]],[[[7,7],[8,8]],[[7,7],[8,7]]]]"),
                d.parseToFish("[7,[5,[[3,8],[1,4]]]]")
            )
        )
    }

    @Test
    fun testAddAndReduceAll() {
        val d = Day18()
        assertLike(
            d.parseToFish("[[[[1,1],[2,2]],[3,3]],[4,4]]"),
            d.addAndReduceAll(listOf("[1,1]", "[2,2]", "[3,3]", "[4,4]").map { d.parseToFish(it) })
        )
        assertLike(
            d.parseToFish("[[[[3,0],[5,3]],[4,4]],[5,5]]"),
            d.addAndReduceAll(listOf("[1,1]", "[2,2]", "[3,3]", "[4,4]", "[5,5]").map { d.parseToFish(it) })
        )
        assertLike(
            d.parseToFish("[[[[5,0],[7,4]],[5,5]],[6,6]]"),
            d.addAndReduceAll(listOf("[1,1]", "[2,2]", "[3,3]", "[4,4]", "[5,5]", "[6,6]").map { d.parseToFish(it) })
        )
        assertLike(
            d.parseToFish("[[[[8,7],[7,7]],[[8,6],[7,7]]],[[[0,7],[6,6]],[8,7]]]"),
            d.addAndReduceAll(listOf(
                "[[[0,[4,5]],[0,0]],[[[4,5],[2,6]],[9,5]]]",
                "[7,[[[3,7],[4,3]],[[6,3],[8,8]]]]",
                "[[2,[[0,8],[3,4]]],[[[6,7],1],[7,[1,6]]]]",
                "[[[[2,4],7],[6,[0,5]]],[[[6,8],[2,8]],[[2,1],[4,5]]]]",
                "[7,[5,[[3,8],[1,4]]]]",
                "[[2,[2,2]],[8,[8,1]]]",
                "[2,9]",
                "[1,[[[9,3],9],[[9,0],[0,7]]]]",
                "[[[5,[7,4]],7],1]",
                "[[[[4,2],2],6],[8,7]]"
            ).map { d.parseToFish(it) })
        )
    }

    @Test
    fun testPart1() {
        val lines = readLines("testDay18")
        assertEquals(4140, Day18().part1(lines))
    }

    @Test
    fun testPart2() {
        val lines = readLines("testDay18")
        assertEquals(3993, Day18().part2(lines))
    }

    private fun assertLike(fishExpected: List<Day18.E>, fishActual: List<Day18.E>) {
        assertTrue(
            fishExpected == fishActual,
            "Fishes are not the same\nExpected: ${Day18().fishToString(fishExpected)}\nActual:   ${Day18().fishToString(fishActual)}"
        )
    }
}