class WordCalculatorTest extends org.scalatest.FunSuite {
    test("WordCalculatorTest.removeLetter") {
        assert(WordCalculator.removeLetter("oscar", 2) === "ocar")
    }

    test("WordCalculatorTest.expand") {
        assert(WordCalculator.expand("cat") === Vector("at","ct","ca"))
        assert(WordCalculator.expand("at") === Vector("t","a"))
    }

    test("WordCalculatorTest.combinations") {
        assert(WordCalculator.combinations("cat") === Vector("cat", "cta", "act", "atc", "tca", "tac"))
    }


    test("WordCalculatorTest.expandFilter.test0") {
        assert(WordCalculator.expandFilter("at") === List("a"))
        assert(WordCalculator.expandFilter2("at") === List("a"))
    }

    test("WordCalculatorTest.expandFilter.test1") {
        // 'at' is missing from the dictionary...
        assert(WordCalculator.expandFilter("cat") === List("ac"))
        assert(WordCalculator.expandFilter2("cat") === List("ac"))
    }

    test("WordCalculatorTest.expandFilter.test2") {
        assert(WordCalculator.expandFilter("radio") === List("oria", "road", "ador", "orad", "arid"))
        assert(WordCalculator.expandFilter2("radio") === List("oria", "road", "ador", "orad", "arid"))
    }

    test("WordCalculatorTest.recCombinations") {
        assert(WordCalculator.recCombinations("a") === List(List("a")))
        assert(WordCalculator.recCombinations("at") === List(List("at","a")))
        // 'at' is missing from the dictionary...
        assert(WordCalculator.recCombinations("cat") === List(List("cat","ac","a")))
    }
}
