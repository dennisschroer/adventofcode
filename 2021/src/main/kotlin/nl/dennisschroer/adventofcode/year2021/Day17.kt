package nl.dennisschroer.adventofcode.year2021

import java.lang.Integer.min

class Day17 {
    fun part1(xs: IntRange, ys: IntRange): Int {
        val succesfullProbes = mutableListOf<Pair<Int, Int>>()

        // Just try some velocities
        (0..100).forEach { vx ->
            (0..100).forEach { vy ->
                var t = 0
                while (y(vy, t) > -100) { // -100 is enough boundary for now

                    if (ys.contains(y(vy, t)) && xs.contains(x(vx, t))) {
                        println("Initial velocity $vx,$vy \treaches ${x(vx, t)},${y(vy, t)} \tafter time $t WITHIN TARGET!")
                        succesfullProbes.add(vx to vy)
                    } else {
                        println("Initial velocity $vx,$vy \treaches ${x(vx, t)},${y(vy, t)} \tafter time $t")
                    }
                    t++
                }
            }
        }

        return succesfullProbes.maxOf { (_, vy) -> maxY(vy) }
    }

    /** Horizontal position of the probe with initial velocity vx after time t */
    private fun x(vx: Int, t: Int) = (vx + (vx - min(vx, t) + 1)) * min(vx, t) / 2

    /** Vertical position of the probe with initial velocity vy after time t */
    private fun y(vy: Int, t: Int) = (vy + (vy - t + 1)) * t / 2

    /** Calculates the maximum height for given intial vertical velocity */
    private fun maxY(vy: Int) = (vy + 1) * vy / 2

    fun part2(input: List<String>): Int {
        return -1
    }
}

fun main() {
    println("Part 1: ${Day17().part1(287..309, -76..-48)}")
    println("Part 2: ${Day17().part2(readLines("day17"))}")
}