package day3

class Day3 {
    public static final int HEIGHT = 1000
    public static final int WIDTH = 1000

    static List<String> getInput() {
        getClass().getResource("/day3/claims.txt").readLines()
    }

    static Map<Integer, Map<Integer, Set<Integer>>> plotFabric(List<String> input) {
        Map<Integer, Map<Integer, Set<Integer>>> fabric = initFabric()

        input.each {
            //#1 @ 555,891: 18x12
            def (claim, startX, startY, width, height) = it.substring(1).split("[@,:x ]+").collect { it.toInteger() }

            (startX..(startX + width - 1)).each { x ->
                (startY..(startY + height - 1)).each { y ->
                    fabric.get(x).get(y).add(claim)
                }
            }
        }

        fabric
    }

    static List<Integer> claimsWithoutOverlap(List<String> input) {
        List<Integer> result = []
        Map<Integer, Map<Integer, Set<Integer>>> fabric = plotFabric(input)

        input.each {
            //#1 @ 555,891: 18x12
            def (claim, startX, startY, width, height) = it.substring(1).split("[@,:x ]+").collect { it.toInteger() }

            boolean hasOverlap = false

            (startX..(startX + width - 1)).each { x ->
                (startY..(startY + height - 1)).each { y ->
                    if (fabric.get(x).get(y).size() > 1) {
                        hasOverlap = true
                    }
                }
            }

            if (!hasOverlap) {
                result.add(claim)
            }
        }

        result
    }

    static Map<Integer, Map<Integer, Set<Integer>>> initFabric() {
        Map<Integer, Map<Integer, Set<Integer>>> result = [:]

        (0..WIDTH - 1).each { x ->
            result.put(x, [:])
            (0..HEIGHT - 1).each { y ->
                result.get(x).put(y, new HashSet<>())
            }
        }

        result
    }

    static int calculateNumberOfOverlaps(List<String> input) {
        Map<Integer, Map<Integer, Set<Integer>>> fabric = plotFabric(input)

        fabric.values().collect { it.values().collect { it.size() } }.flatten().count { it > 1 }
    }


    static void main(String[] args) {
        def input = getInput()

        println "1st half: " + calculateNumberOfOverlaps(input)
        println "2nd half: " + claimsWithoutOverlap(input)
    }
}


