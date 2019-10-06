object WordCalculator extends App {

    import scala.io.Source

    // if lazy is removed then words is not initialized for unit tests
    lazy val words = Source.fromResource("words.txt").getLines.toList.toSet

    val rc = recCombinations("window")
    val rcSorted = rc.map(l => (l.length, l)).sortBy(-_._1)
    println(rcSorted)

    def removeLetter(w: String, i: Int) = w.substring(0, i - 1) + w.substring(i, w.length)

    def expand(w: String) = (1 to w.length) map (l => removeLetter(w, l))

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

    def recCombinations(w: String): List[List[String]] = {

        val nw = expandFilter(w)
        println(nw)
        nw match {
            case _ :: _ => nw.flatMap(w2 => (recCombinations(w2).map(lw => w :: lw)))
            case _ => List(nw)
        }

    }
}