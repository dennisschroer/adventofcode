package day2

class Day2 {
    static List<String> getInput() {
        getClass().getResource("/day2/ids.txt").readLines()
    }

    static int calculateChecksum(List<String> ids) {
        def numberOfDoubles = 0
        def numberOfTriples = 0

        ids.forEach({
            def counts = it.toCharArray().toList().groupBy { it }.collect { key, value -> value.size() }
            if (counts.contains(2)) {
                println "$it contains a double"
                numberOfDoubles++
            }
            if (counts.contains(3)) {
                println "$it contains a triple"
                numberOfTriples++
            }
        })

        println "Number of doubles: $numberOfDoubles"
        println "Number of triples: $numberOfTriples"

        return numberOfDoubles * numberOfTriples
    }

    static String findCommonOfCorrectBoxes(List<String> ids){
        def length = ids[0].length()
        def count = ids.size()
        def result = null

        ids.eachWithIndex { String id, int position ->
            if(position < count-1){
                (position+1..count-1).each {
                    def common = commonLetters(id, ids[it])
                    println "$id | ${ids[it]} : $common (${common.length()})"
                    if(common.length() == length - 1){
                        result = common
                    }
                }
            }
        }

        result
    }

    static String commonLetters(String id1, String id2) {
        (0..id1.length()-1).collect { id1[it] == id2[it] ? id1[it] : null }.findAll { it != null }.join()
    }

    static void main(String[] args) {
        def input = getInput()

        println "1st half: " + calculateChecksum(input)
        println "2nd half: " + findCommonOfCorrectBoxes(input)
    }
}


