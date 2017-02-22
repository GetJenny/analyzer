import com.getjenny.analyzer._

object testone {

  val k = new Keyword("ciao")

  assert(k.evaluate("ciao stupido cazzone") == 1.0/3)
  assert(k.matches("ciao stupido cazzone"))

  val anal = new DefaultAnalyzer("""and ( keyword ("ciao"), or ( keyword ("stupido"), keyword ("idiota")  )  )""")
  assert(anal.evaluate("ciao stupido cazzone") == 1)
  assert(anal.evaluate("ciao cazzone idiota") == 1)
  assert(anal.evaluate("stupido cazzone idiota") == 0)


}