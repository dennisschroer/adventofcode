package nl.dennisschroer.adventofcode.year2021

import org.junit.jupiter.api.Assertions.assertEquals
import org.junit.jupiter.api.Test
import kotlin.test.assertContains

internal class Day22Test {

    @Test
    fun testIntRangeLength() {
        assertEquals(6, (5..10).length)
        assertEquals(11, (-5..5).length)
        assertEquals(1, (5..5).length)
        assertEquals(0, (5..-5).length)
    }

    @Test
    fun testSubtractRegionPartialOverlap() {
        val regionToSplit = Day22.Region(0..10, 0..10, 0..10)
        val overlap = Day22.Region(5..10, 5..10, 5..10)

        val result = Day22().subtractRegion(regionToSplit, overlap)

        assertEquals(7, result.size)
        assertContains(result, Day22.Region(0..4, 0..4, 0..4))
        assertContains(result, Day22.Region(0..4, 0..4, 5..10))
        assertContains(result, Day22.Region(0..4, 5..10, 0..4))
        assertContains(result, Day22.Region(0..4, 5..10, 5..10))
        assertContains(result, Day22.Region(5..10, 0..4, 0..4))
        assertContains(result, Day22.Region(5..10, 0..4, 5..10))
        assertContains(result, Day22.Region(5..10, 5..10, 0..4))
    }

    @Test
    fun testSubtractRegionPartialOverlap2() {
        val regionToSplit = Day22.Region(10..12, 10..12, 10..12)
        val overlap = Day22.Region(11..13,11..13,11..13)

        val result = Day22().subtractRegion(regionToSplit, overlap)

        assertEquals(7, result.size)
        assertContains(result, Day22.Region(10..10, 10..10, 10..10))
        assertContains(result, Day22.Region(10..10, 10..10, 11..12))
        assertContains(result, Day22.Region(10..10, 11..12, 10..10))
        assertContains(result, Day22.Region(10..10, 11..12, 11..12))
        assertContains(result, Day22.Region(11..12, 10..10, 10..10))
        assertContains(result, Day22.Region(11..12, 10..10, 11..12))
        assertContains(result, Day22.Region(11..12, 11..12, 10..10))
    }

    @Test
    fun testSubtractRegionCompleteOverlap() {
        val regionToSplit = Day22.Region(0..10, 0..10, 0..10)
        val overlap = Day22.Region(-10..20, -10..20, -10..20)

        val result = Day22().subtractRegion(regionToSplit, overlap)

        assertEquals(0, result.size)
    }

    @Test
    fun testSubtractRegionCompleteInside() {
        val regionToSplit = Day22.Region(0..10, 0..10, 0..10)
        val overlap = Day22.Region(2..8, 2..8, 2..8)

        val result = Day22().subtractRegion(regionToSplit, overlap)

        assertEquals(26, result.size)
    }

    @Test
    fun testSimpleExample() {
        assertEquals(39, Day22().part1(readLines("testDay22_simple")))
    }

    @Test
    fun testPart1() {
        assertEquals(590784, Day22().part1(readLines("testDay22")))
    }

    @Test
    fun testPart2() {
        assertEquals(2758514936282235, Day22().part2(readLines("testDay22_pt2")))
    }
}