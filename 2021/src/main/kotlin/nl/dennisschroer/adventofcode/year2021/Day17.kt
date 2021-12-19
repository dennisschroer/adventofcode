package nl.dennisschroer.adventofcode.year2021

import java.lang.Integer.min

class Day17 {
    fun part1(xs: IntRange, ys: IntRange): Int {
        return findSuccesfullProbes(ys, xs).maxOf { (_, vy) -> maxY(vy) }
    }

    fun part2(xs: IntRange, ys: IntRange): Int {
        return findSuccesfullProbes(ys, xs).size
    }

    private fun findSuccesfullProbes(ys: IntRange, xs: IntRange): Set<Pair<Int, Int>> {
        val succesfullProbes = mutableSetOf<Pair<Int, Int>>()

        // Just try some velocities
        (0..xs.last).forEach { vx ->
            (-100..100).forEach { vy ->
                var t = 0

                // If we overshoot abort immediately
                while (y(vy, t) >= ys.first && x(vx, t) <= xs.last) {
                    if (ys.contains(y(vy, t)) && xs.contains(x(vx, t))) {
                        println("Initial velocity $vx,$vy \treaches ${x(vx, t)},${y(vy, t)} \tafter time $t WITHIN TARGET!")
                        succesfullProbes.add(vx to vy)
                    }
                    t++
                }
            }
        }
        return succesfullProbes
    }

    /** Horizontal position of the probe with initial velocity vx after time t */
    private fun x(vx: Int, t: Int) = (vx + (vx - min(vx, t) + 1)) * min(vx, t) / 2

    /** Vertical position of the probe with initial velocity vy after time t */
    private fun y(vy: Int, t: Int) = (vy + (vy - t + 1)) * t / 2

    /** Calculates the maximum height for given initial vertical velocity */
    private fun maxY(vy: Int) = (vy + 1) * vy / 2
}

fun main() {
    println("Part 1: ${Day17().part1(287..309, -76..-48)}")
    println("Part 2: ${Day17().part2(287..309, -76..-48)}")
}


//7,-1
//8,-1
//8,-2
//9,-1
//9,-2
//10,-1
//10,-2
//11,-1
//11,-2
//11,-3
//11,-4
//12,-2
//12,-3
//12,-4
//13,-2
//13,-3
//13,-4
//14,-2
//14,-3
//14,-4
//15,-2
//15,-3
//15,-4
//20,-5
//20,-6
//20,-7
//20,-8
//20,-9
//20,-10
//21,-5
//21,-6
//21,-7
//21,-8
//21,-9
//21,-10
//22,-5
//22,-6
//22,-7
//22,-8
//22,-9
//22,-10
//23,-5
//23,-6
//23,-7
//23,-8
//23,-9
//23,-10
//24,-5
//24,-6
//24,-7
//24,-8
//24,-9
//24,-10
//25,-5
//25,-6
//25,-7
//25,-8
//25,-9
//25,-10
//26,-5
//26,-6
//26,-7
//26,-8
//26,-9
//26,-10
//27,-5
//27,-6
//27,-7
//27,-8
//27,-9
//27,-10
//28,-5
//28,-6
//28,-7
//28,-8
//28,-9
//28,-10
//29,-5
//29,-6
//29,-7
//29,-8
//29,-9
//29,-10
//30,-5
//30,-6
//30,-7
//30,-8
//30,-9
//30,-10
//6,0
//6,1
//6,2
//6,3
//6,4
//6,5
//6,6
//6,7
//6,8
//6,9
//7,0
//7,1
//7,2
//7,3
//7,4
//7,5
//7,6
//7,7
//7,8
//7,9
//8,0
//8,1
//9,0