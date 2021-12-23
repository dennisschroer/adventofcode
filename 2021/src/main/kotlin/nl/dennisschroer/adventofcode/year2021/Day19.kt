package nl.dennisschroer.adventofcode.year2021

import kotlin.math.abs

class Day19 {
    data class Scanner(
        val number: Int,
        var detectedBeacons: Set<Beacon>,
        var position: Beacon? = null,
        var transformation: ((Beacon) -> Beacon)? = null
    )

    data class Beacon(val x: Int, val y: Int, val z: Int) : Comparable<Beacon> {
        fun plus(other: Beacon): Beacon = Beacon(this.x + other.x, this.y + other.y, this.z + other.z)

        override fun compareTo(other: Beacon): Int {
            return if (this.x != other.x) {
                this.x - other.x
            } else if (this.y != other.y) {
                this.y - other.y
            } else {
                this.z - other.z
            }
        }

        override fun toString(): String = "$x,$y,$z"
    }

    // Rotations while facing z
    private val rotations: List<(Beacon) -> Beacon> =
        listOf(
            { Beacon(it.x, it.y, it.z) },
            { Beacon(it.y, -it.x, it.z) },
            { Beacon(-it.x, -it.y, it.z) },
            { Beacon(-it.y, it.x, it.z) }
        )

    private val orientations: List<(Beacon) -> Beacon> =
        listOf(
            { Beacon(it.x, it.y, it.z) },
            { Beacon(it.z, it.y, -it.x) },
            { Beacon(-it.x, it.y, -it.z) },
            { Beacon(-it.z, it.y, it.x) },
            { Beacon(it.x, it.z, -it.y) },
            { Beacon(it.x, -it.z, it.y) }
        )

    private val transformations: List<(Beacon) -> Beacon> =
        orientations.flatMap { orientate -> rotations.map { rotate -> { rotate(orientate(it)) } } }

    private fun alignScanners(scanners: List<Scanner>) {
        while (scanners.find { it.position == null } != null) {
            scanners.filter { it.position != null }.forEach { fixedScanner ->
                scanners.filter { it.position == null }.forEach { scannerB ->
                    transformations.forEach { transformation ->
                        val transformedBeacons = scannerB.detectedBeacons.map(transformation)

                        val beaconPairs = fixedScanner.detectedBeacons.flatMap { a -> transformedBeacons.map { b -> a to b } }
                        val distances = beaconPairs.map { (a, b) -> Beacon(a.x - b.x, a.y - b.y, a.z - b.z) }
                        val distanceCounts = distances.groupingBy { it }.eachCount().filterValues { it >= 12 }
                        if (distanceCounts.isNotEmpty()) {
                            assert(distanceCounts.size == 1)

                            val relativeDistance = distanceCounts.keys.first()

                            scannerB.position = relativeDistance
                            scannerB.transformation = transformation
                            scannerB.detectedBeacons = transformedBeacons.map { it.plus(relativeDistance) }.toSet()

                            val demoTransformation = transformation(Beacon(0, 1, 2))
                            println("Matched scanner ${fixedScanner.number} and ${scannerB.number} with relative distance $relativeDistance and transformation $demoTransformation. Scanner ${scannerB.number} is on position ${scannerB.position}")
                        }
                    }
                }
            }
        }
    }

    fun part1(input: String): Int {
        val scanners = readScanners(input)
        alignScanners(scanners)

        val allBeacons = scanners.flatMap { it.detectedBeacons }.toSortedSet()

        println("Full list of beacons:")
        allBeacons.forEach { println(it) }

        return allBeacons.size
    }

    fun part2(input: String): Int {
        val scanners = readScanners(input)
        alignScanners(scanners)

        return scanners.flatMap { a ->
            scanners.map { b ->
                abs(a.position!!.x - b.position!!.x) + abs(a.position!!.y - b.position!!.y) + abs(a.position!!.z - b.position!!.z)
            }
        }.maxOf { it }
    }

    private fun readScanners(input: String): List<Scanner> {
        val scanners = input.split("\n\n").mapIndexed { index, lines ->
            Scanner(index, lines.split("\n").drop(1).map { readBeacon(it) }.toSet())
        }

        // For the first scanner, the position is known
        scanners[0].position = Beacon(0, 0, 0)
        scanners[0].transformation = { it }

        return scanners
    }

    private fun readBeacon(line: String): Beacon {
        return line.split(",").let { Beacon(it[0].toInt(), it[1].toInt(), it[2].toInt()) }
    }
}

fun main() {
    println("Part 1: ${Day19().part1(readFile("day19"))}")
    println("Part 2: ${Day19().part2(readFile("day19"))}")
}