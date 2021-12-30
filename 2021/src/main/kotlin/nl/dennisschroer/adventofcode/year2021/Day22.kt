package nl.dennisschroer.adventofcode.year2021

import kotlin.math.max
import kotlin.math.min


class Day22 {
    data class RebootStep(val command: String, val region: Region)
    data class Region(val xs: IntRange, val ys: IntRange, val zs: IntRange) {
        fun overlaps(other: Region) = xs.overlaps(other.xs) && ys.overlaps(other.ys) && zs.overlaps(other.zs)
        fun size(): Long = xs.length.toLong() * ys.length * zs.length
    }

    fun part1(input: List<String>): Long {
        return runInitializationProcedure(parseRebootSteps(input, -50, 50))
    }

    fun part2(input: List<String>): Long {
        return runInitializationProcedure(parseRebootSteps(input, Int.MIN_VALUE, Int.MAX_VALUE))
    }

    private fun runInitializationProcedure(steps: List<RebootStep>): Long {
        // List of regions where all coordinates are on
        val regions = mutableListOf<Region>()

        steps.forEachIndexed { index, step ->
            // Find regions with overlap
            regions.filter { it.overlaps(step.region) }.forEach { overlappingRegion ->
                val newRegions = subtractRegion(overlappingRegion, step.region)

                // Add only the sections without overlap
                regions.remove(overlappingRegion)
                regions.addAll(newRegions)
            }

            // Only when the new step turns coordinates on, add it
            if (step.command == "on") {
                regions.add(step.region)
            }

            println("Step ${index + 1} of ${steps.size}; Regions: ${regions.size}, on: ${regions.sumOf { it.size() }}")

            // Assert step
            regions.forEachIndexed { i, a ->
                regions.drop(i + 1).forEach { b ->
                    assert(!a.overlaps(b)) {
                        "$a overlaps with $b"
                    }
                }
            }
        }

        return regions.sumOf { it.size() }
    }

    fun subtractRegion(region: Region, overlapping: Region): List<Region> {
        // Split the region in 27 sections
        val newRegions = mutableSetOf<Region>()

        listOf((region.xs.first until overlapping.xs.first), (max(region.xs.first, overlapping.xs.first)..min(region.xs.last, overlapping.xs.last)), (overlapping.xs.last + 1..region.xs.last)).forEach { xs ->
            listOf((region.ys.first until overlapping.ys.first), (max(region.ys.first, overlapping.ys.first)..min(region.ys.last, overlapping.ys.last)), (overlapping.ys.last + 1..region.ys.last)).forEach { ys ->
                listOf((region.zs.first until overlapping.zs.first), (max(region.zs.first, overlapping.zs.first)..min(region.zs.last, overlapping.zs.last)), (overlapping.zs.last + 1..region.zs.last)).forEach { zs ->
                    newRegions.add(Region(xs, ys, zs))
                }
            }
        }

        return newRegions.filter { it.size() > 0 }.filter { !it.overlaps(overlapping) }

//
//        // Split the region in 8 sections
//        val splitPoint = Triple(
//            if (region.xs.contains(overlapping.xs.first)) overlapping.xs.first else overlapping.xs.last + 1,
//            if (region.ys.contains(overlapping.ys.first)) overlapping.ys.first else overlapping.ys.last + 1,
//            if (region.zs.contains(overlapping.zs.first)) overlapping.zs.first else overlapping.zs.last + 1,
//        )
//
//        return listOf(
//            Region(
//                region.xs.first until splitPoint.first,
//                region.ys.first until splitPoint.second,
//                region.zs.first until splitPoint.third,
//            ),
//            Region(
//                splitPoint.first..region.xs.last,
//                region.ys.first until splitPoint.second,
//                region.zs.first until splitPoint.third,
//            ),
//            Region(
//                region.xs.first until splitPoint.first,
//                splitPoint.second..region.ys.last,
//                region.zs.first until splitPoint.third,
//            ),
//            Region(
//                splitPoint.first..region.xs.last,
//                splitPoint.second..region.ys.last,
//                region.zs.first until splitPoint.third,
//            ),
//            Region(
//                region.xs.first until splitPoint.first,
//                region.ys.first until splitPoint.second,
//                splitPoint.third..region.zs.last,
//            ),
//            Region(
//                splitPoint.first..region.xs.last,
//                region.ys.first until splitPoint.second,
//                splitPoint.third..region.zs.last,
//            ),
//            Region(
//                region.xs.first until splitPoint.first,
//                splitPoint.second..region.ys.last,
//                splitPoint.third..region.zs.last,
//            ),
//            Region(
//                splitPoint.first..region.xs.last,
//                splitPoint.second..region.ys.last,
//                splitPoint.third..region.zs.last,
//            )
//        ).filter { it.size() > 0 }
    }

    private fun parseRebootSteps(input: List<String>, min: Int, max: Int): List<RebootStep> {
        return input.map { line ->
            val command = line.substringBefore(" ")
            val coordinates = line.substringAfter(" ").split(",").map { it.drop(2) }

            RebootStep(
                command,
                Region(
                    toIntRange(coordinates[0], min, max),
                    toIntRange(coordinates[1], min, max),
                    toIntRange(coordinates[2], min, max)
                )
            )
        }.filter { it.region.size() > 0 }
    }

    private fun toIntRange(rangeDefinition: String, min: Int, max: Int): IntRange =
        max(rangeDefinition.substringBefore(".").toInt(), min)..min(rangeDefinition.substringAfterLast(".").toInt(), max)
}

fun main() {
    println("Part 1: ${Day22().part1(readLines("day22"))}")
    println("Part 2: ${Day22().part2(readLines("day22"))}")
}