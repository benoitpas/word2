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
        assert(WordCalculator.expandFilter("at") === List())
        assert(WordCalculator.expandFilter2("at") === List())
    }

    test("WordCalculatorTest.expandFilter.test1") {
        // 'at' is missing from the dictionary...
        assert(WordCalculator.expandFilter("cat") === List("at","ta"))
        assert(WordCalculator.expandFilter2("cat") === List("at","ta"))
    }

    test("WordCalculatorTest.expandFilter.test2") {
        assert(WordCalculator.expandFilter("radio") === List("road", "orad", "raid", "arid"))
        assert(WordCalculator.expandFilter2("radio") === List("road", "orad", "raid", "arid"))
    }

    test("WordCalculatorTest.recCombinations") {
        assert(WordCalculator.recCombinations("a") === List(List("a")))
        assert(WordCalculator.recCombinations("at") === List(List("at")))
        assert(WordCalculator.recCombinations("cat") === List(List("cat","at"),List("cat","ta")))
    }
/*
    test("WordCalculatorTest.funnel2") {
        assert(WordCalculator.funnel2("gnash") == 4)
        assert(WordCalculator.funnel2("princesses") == 9)
//        assert(WordCalculator.funnel2("turntables") == 5)
        assert(WordCalculator.funnel2("implosive") == 1)
 //       assert(WordCalculator.funnel2("programmer") == 2)
    }
    */
}
