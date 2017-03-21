package korolev.template

import korolev.template.util.ToKebabCase

final class TemplateContext[Const: ToKebabCase, MiscType] {

  sealed trait Template

  sealed trait Document extends Template {

    import Document._

    /** Converts document to programmatic template */
    def toProgrammaticTemplate(implicit context: RenderContext): ProgrammaticTemplate = {
      // Converts camelCase to kebab-case
      // toProgrammaticTemplate is a member of Document
      // templateToProgrammatic converts any template to
      // programmatic template if it possible
      def templateToProgrammatic(template: Template) = template match {
        case d: Document => d.toProgrammaticTemplate(context).run()
        case pt: ProgrammaticTemplate => pt.run()
        case _ => // do nothing
      }
      ProgrammaticTemplate {
        this match {
          case Empty => // do nothing
          case misc: Misc => context.addMisc(misc)
          case Attr(name, value) => context.setAttr(toKebabCase(name), value)
          case Text(value) => context.addTextNode(value)
          case Fragment(xs) => xs.foreach(templateToProgrammatic)
          case Node(name, xs) =>
            context.openNode(toKebabCase(name))
            xs.foreach(templateToProgrammatic)
            context.closeNode()
        }
      }
    }
  }

  object Document {
    sealed trait NodeLike extends Document
    /** HTML tag. Like `<div></div>` */
    case class Node(name: Const, children: Seq[Template]) extends NodeLike
    /** Tag's attribute. Like `<div class="header"></div>` */
    case class Attr(name: Const, value: Const) extends Document
    /** Text inside tag. Like `<h1>Hello world</h1>`*/
    case class Text(value: Const) extends Document
    /** Nodes which are have to be added as a children of parent node.  */
    case class Fragment(xs: Seq[NodeLike]) extends NodeLike
    /** A way to add something specific to template. Events, delays, whatever. */
    case class Misc(value: MiscType) extends Document
    /** Just emptiness. */
    case object Empty extends Document
  }

  final class ProgrammaticTemplate(f: => Unit) extends Template {
    /** Executes the template */
    def run(): Unit = f
  }

  object ProgrammaticTemplate {
    def apply[T](f: => Unit): ProgrammaticTemplate =
      new ProgrammaticTemplate(f)
  }

  trait RenderContext {
    def openNode(name: Const): Unit
    def closeNode(): Document.Node
    def setAttr(name: Const, value: Const): Document.Attr
    def addTextNode(text: Const): Document.Text
    def addMisc(misc: Document.Misc): Unit
  }

  object RenderContext {
    object Dummy extends RenderContext {
      val FakeNode = Document.Node("fake".asInstanceOf[Const], Nil)
      val FakeText = Document.Text("fake".asInstanceOf[Const])
      val FakeAttr = Document.Attr("fake".asInstanceOf[Const], "fake".asInstanceOf[Const])
      def openNode(name: Const): Unit = ()
      def closeNode(): Document.Node = FakeNode
      def setAttr(name: Const, value: Const): Document.Attr = FakeAttr
      def addTextNode(text: Const): Document.Text = FakeText
      def addMisc(misc: Document.Misc): Unit = ()
    }
  }

  val toKebabCase = ToKebabCase[Const].convert _
}
