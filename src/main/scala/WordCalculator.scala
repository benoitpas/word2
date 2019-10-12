object WordCalculator extends App {

    /**
      * Naive implementation, does not scale (returns all combinations)
      * As only the longest is necessary, using backtracking to find only 1 combination is probably enough (and more efficient !)
      */

    import scala.io.Source

    // if lazy is removed then words is not initialized for unit tests
    lazy val words = Source.fromResource("enable1.txt").getLines.toList.toSet

    val rc = recCombinations("apple")
    val rcSorted = rc.map(l => (l.length, l)).sortBy(-_._1)
    println(rcSorted)
    println(funnel2("bbbb"))

    def removeLetter(w: String, i: Int): String = w.substring(0, i - 1) + w.substring(i, w.length)

    def expand(w: String): IndexedSeq[String] = (1 to w.length) map (l => removeLetter(w, l))

    // Would It be better to return a Set to remove duplicates ?
    def combinations(w: String): IndexedSeq[String] = (1 to w.length) flatMap ((i: Int) => {
        val letter: String = w.charAt(i - 1).toString
        val combs = combinations(removeLetter(w, i)).toList
        combs match {
            case List() => List(letter)
            case _ => combs map (letter + _)
        }
    })

    def expandFilter(w: String) = {
        val nw = (expand(w) flatMap combinations).filter(words.contains(_))
        nw.toList
    }

    def expandFilter2(w: String): IndexedSeq[String] = {
        for (w2 <- expand(w);
             w3 <- combinations(w2)
             if words.contains(w3))
            yield w3
    }

    def recCombinations(w: String): List[List[String]] = {

        val nw = expandFilter(w)
        nw match {
            case _ :: _ => nw.flatMap(w2 => (recCombinations(w2).map(lw => w :: lw)))
            case _ => List(List(w))
        }

    }

    // funnel that also changes the order of the letters at each step !!
    def funnel2(w: String): Int = {
        val r = recCombinations(w).map(l => (l.length, l)).sortBy(-_._1)
        r match {
            case h :: _ => h._1
            case _ => 0 // shouldn't happen...
        }
    }

}