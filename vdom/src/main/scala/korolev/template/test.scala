package korolev.template

/**
  * @author Aleksey Fomkin <aleksey.fomkin@gmail.com>
  */
object test {
  val dsl = new TemplateDsl(new TemplateContext[String, Any]())
}
