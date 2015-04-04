object MatchSyntax {

  def main(args: Array[String]) {
    def strMatch(a: Any) = a match {
      case s:String => println("string: "+s);
      case x => println("not string: "+x);
    }

    val strFun: Any => Unit = {
      case s: String => println("string: "+s);
      case x => println("not string: "+x);
    }

    strMatch("asdf")
    strFun("asdf")
  }

}
