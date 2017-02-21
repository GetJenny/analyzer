import com.getjenny.analyzer._

object testone {

  val k = new Keyword("ciao")
  println("try a keyword: " + k)

  k.evaluate("ciao stupido cazzone")
  k.matches("ciao stupido cazzone")

  val anal = new DefaultAnalyzer("keyword(\"stupido\")")



}