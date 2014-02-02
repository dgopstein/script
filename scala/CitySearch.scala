//TODO Dont fuzzy match when fewer than 3 characters typed

import scala.util.Try

object Util {
  def time[R](block: => R): R = {
    val t0 = System.nanoTime()
    val result = block    // call-by-name
    val t1 = System.nanoTime()
    println(f"Elapsed time: ${(t1 - t0)/1000000d}%.2f ms")
    result
  }

  def toBigrams(str: String): Seq[String] = if (str.length < 2) Nil else str.sliding(2).toSeq

  def stringSimilarity(a: String, b: String): Float = {
      val x = toBigrams(a)
      val y = toBigrams(b)
      1 - ((x intersect y).size * 2) / (x.size + y.size).toFloat
  }

}

case class Trie[T](nodes: Map[Char, Trie[T]] = Map[Char, Trie[T]](), vals: Seq[T] = Nil) {
  def nonEmpty = vals.nonEmpty

  def updated(nodes: Map[Char, Trie[T]] = nodes, vals: Seq[T] = vals) =
    Trie[T](nodes, vals)

  def add(key: String, value: T): Trie[T] = {
    key.headOption match {
        case None => updated(vals = vals :+ value)
        case Some(c) => updated(nodes = nodes ++ Map(c -> nodes.getOrElse(c, Trie[T]()).add(key.tail, value)))
    }
  }

  // Find the node corresponding to a prefix (if it exists)
  def get(key: String): Option[Trie[T]] =
    key.headOption match {
        case None => Some(this)
        case Some(c) => nodes.get(c).flatMap(_.get(key.tail))
    }

  // Find all nodes corresponding to a prefix given a certain edit distance
  def getFuzzy(key: String, edits: Int, prefix: String = ""): Iterable[(String, Trie[T])] =
    key.headOption match {
        case None => Seq(prefix -> this)
        case Some(c) => if (edits == 0) nodes.get(c).map(_.getFuzzy(key.tail, edits, prefix + c)).getOrElse(Nil)
                        else nodes.flatMap{ case (k, v) => v.getFuzzy(key.tail, if (k == c) edits else edits - 1, prefix + k) }
    }

  // Return all values below this
  def descendants: Seq[T] = vals ++ nodes.values.view.flatMap(_.descendants)

  // Return all key suffixes below this
  def tails: Iterable[String] = nodes.filter(_._2.nonEmpty).map(_._1.toString) ++ nodes.view.flatMap{case (k, v) => v.tails.map(k + _)}

  // Find all valid completions for a string prefix
  def complete(key: String): Iterable[T] = get(key).map(_.descendants).getOrElse(Nil)
  def completeFuzzy(key: String, edits: Int = 1): Iterable[(String, T)] = getFuzzy(key, edits).flatMap{ case (prefix, trie) => trie.descendants.map(prefix -> _) }
}

object CitySearch {
  case class City(name: String, state: String, pop: Int)

  def listToTrie(seq: Seq[City]) =
    seq.foldLeft(Trie[City]()) { (t, city) =>
      t.add(city.name, city)
    }

  def main(args: Array[String]) = {
    val lines = io.Source.fromFile("/Users/dgopstein/Downloads/uscitiespop.txt").getLines//.filter(_.startsWith("us"))
    val cities = lines.map{l => val s = l.split(","); City(s(1), s(3), Try(s(4).toInt).toOption.getOrElse(0))}.toList
    println("cities: "+cities.take(10))
    val trie = listToTrie(cities)
    println(trie.get("cambridge").map(_.vals))
    println(trie.complete("cambri").take(20))

    for (ln <- io.Source.stdin.getLines) {
        Util.time{println("-strict: "+trie.complete(ln).take(10))}
        Util.time{println("-strict by popularity: "+trie.complete(ln).toVector.sortBy(-_.pop).take(10))}
        Util.time{println("-fuzzy by popularity: "+trie.completeFuzzy(ln).map(_._2).toVector.sortBy(-_.pop).take(10))}
//        Util.time{println("fuzzy: "+trie.completeFuzzy(ln)
//                                        .map(_._2).toVector
//                                        .sortBy(c => -(Util.stringSimilarity(ln, c.name.take(ln.size)) + c.pop/8000000000d))
//                                        .map{ _.name}.take(10))}
    }

  }

}
