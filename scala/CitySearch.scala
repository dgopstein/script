// http://download.geonames.org/export/zip/

// Rank results by popultaion
// Include Fuzzy matching

object Util {
  def time[R](block: => R): R = {
    val t0 = System.nanoTime()
    val result = block    // call-by-name
    val t1 = System.nanoTime()
    println(f"Elapsed time: ${(t1 - t0)/1000000d}%.2f ms")
    result
  }
}

case class Trie[T](nodes: Map[Char, Trie[T]], vals: Seq[T]) {
  def nonEmpty = vals.nonEmpty

  def add(key: String, value: T): Trie[T] = {
    key.headOption match {
        case None => Trie(nodes, vals :+ value)
        case Some(c) => Trie(nodes ++ Map(c -> nodes.getOrElse(c, Trie[T]()).add(key.tail, value)), vals)
    }
  }

  def get(key: String): Option[Trie[T]] =
    key.headOption match {
        case None => Some(this)
        case Some(c) => nodes.get(c).flatMap(_.get(key.tail))
    }

  // Return all values below this
  def descendants: Seq[T] = vals ++ nodes.values.view.flatMap(_.descendants)

  // Return all key suffixes below this
  def tails: Iterable[String] = nodes.filter(_._2.nonEmpty).map(_._1.toString) ++ nodes.view.flatMap{case (k, v) => v.tails.map(k + _)}

  def complete(key: String): Iterable[String] = get(key).map(_.tails).getOrElse(Nil).map(key + _)
}

object Trie {
  def apply[T](): Trie[T] = Trie[T](Map[Char, Trie[T]](), Nil)

}

object CitySearch {

  case class City(name: String, state: String)

  def listToTrie(seq: Iterator[City]): Trie[City] = {
    val trie = Trie.apply[City]()

    seq.foldLeft(trie) { (t: Trie[City], city: City) =>
      t.add(city.name, city)
    }
    
  }

  def main(args: Array[String]) = {
    val lines = io.Source.fromFile("/Users/dgopstein/Downloads/US/US.txt").getLines
    val cities = lines.map{l => val s = l.split("\t"); City(s(2), s(4))}
    println(cities.take(20).toVector)
    val trie = listToTrie(cities)
    println(trie.get("Cambridge").map(_.vals))
    println(trie.complete("Cambri").take(20))

    for (ln <- io.Source.stdin.getLines) Util.time{println(trie.complete(ln).take(20))}


  }

}
