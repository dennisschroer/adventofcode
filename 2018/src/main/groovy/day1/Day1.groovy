package day1

class Day1 {
    static List<Integer> getInput() {
        getClass().getResource("/day1/frequencies.txt").readLines().collect { it.toInteger() }
    }

    static int firstOccurenceOfSum() {
        List<Integer> input = getInput()
        def occurences = new TreeSet([0])
        int index = 0
        int sum = 0
        int change

        while (true) {
            change = input[index]
            sum += change
            if (occurences.contains(sum)) {
                return sum
            }

            occurences.add(sum)
            index++
            index = (index == input.size()) ? 0 : index
        }
    }


    static void main(String[] args) {
        // Input
        def input = getInput()

        // First half
        println "1st half: " + input.sum()

        // Second half
        println "2nd half: " + firstOccurenceOfSum()
    }
}


