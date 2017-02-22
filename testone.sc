import com.getjenny.analyzer._

object testone {

  val k = new Keyword("ciao")
  assert(k.evaluate("ciao, stupido cazzone") == 1.0/3)
  assert(k.matches("ciao stupido cazzone"))

  val anal = new DefaultAnalyzer("""and ( keyword ("ciao"), or ( keyword ("stupido"), keyword ("idiota")  )  )""")
  assert(anal.evaluate("ciao stupido cazzone") == 1)
  assert(anal.evaluate("ciao cazzone idiota") == 1)
  assert(anal.evaluate("stupido cazzone idiota") == 0)

  // either "stupido" or "idiota"
  val analBayes = new DefaultAnalyzer("""disjunction( keyword("stupido"), keyword("idiota") )""")

  val idiota = analBayes.evaluate("ciao bel cazzone idiota")
  val stupido_idiota_long = analBayes.evaluate("ciao stupido cazzone idiota")
  val stupido_idiota_short = analBayes.evaluate("ciao stupido idiota")

  //two is better than one
  assert(stupido_idiota_long > idiota)
  assert(stupido_idiota_short > idiota)

  //finding in short is better than finding in longer
  assert(stupido_idiota_short > stupido_idiota_long)

}