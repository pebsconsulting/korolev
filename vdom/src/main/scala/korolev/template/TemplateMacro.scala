package korolev.template

import korolev.template.util.ToKebabCase

import scala.language.experimental.macros
import scala.reflect.macros.blackbox
import macrocompat.bundle

@bundle class TemplateMacro(val c: blackbox.Context) {

  import c.universe._
  /** c.Tree instance of ToKebabCase. Used in compile time */

  implicit val treeToKebabCase: ToKebabCase[c.Tree] = new ToKebabCase[c.Tree] {
    val stringKebab = ToKebabCase[String]
    def convert(tree: c.Tree): c.Tree = tree match {
      case Literal(Constant(value: String)) =>
        val kebabed = stringKebab.convert(value)
        Literal(Constant(kebabed))
      case _ => c.abort(tree.pos, "This can be literal only")
    }
  }

  val tc = new TemplateContext[c.Tree, c.Tree]
  val d = tc.Document


//  def transform(tree: c.Tree)(f: PartialFunction[c.Tree, c.Tree]): c.Tree = {
//    val liftedF = f.lift
//    def aux(tree: c.Tree): c.Tree =
//      liftedF(tree).fold(tree.)
//  }

  def toProgrammaticTemplate(document: c.Tree): c.Tree = {
    // Scope of TemplateDsl instance
    val prefix = c.prefix.tree

    val selectDsl = extractor { case Select(p, TermName(termName)) if p.equalsStructure(prefix) => termName }
    val selectSymbolOps = checker { case selectDsl("SymbolOps") => () }
    val selectSymbol = checker { case Select(Ident(scala), TermName("Symbol")) => () }
    val stringLiteral = extractor { case Literal(Constant(const: String)) => const }

    val createSymbol = extractor {
      case Apply(Select(selectSymbol(), TermName("apply")), List(name @ stringLiteral(_))) =>
        name
    }

    val applySelectSymbolOpsFun = extractor {
      case Apply(Select(Apply(selectSymbolOps(), List(createSymbol(name))), TermName(fun)), value) =>
        (name, fun, value)
    }

    def tcType(name: String) = {
      val tree = q"null: $prefix.templateContext.${TypeName(name)}"
      c.typecheck(tree).tpe
    }
    val programmaticTemplateType = tcType("ProgrammaticTemplate")
    val documentType = tcType("Document")

    object transformer extends Transformer {

      override def transform(tree: c.universe.Tree): c.universe.Tree = tree match {
        case applySelectSymbolOpsFun(name, "apply", xs) =>
          val nested = xs.map(transform)
          val kebabedName = treeToKebabCase.convert(name)
          q"_rc.openNode($kebabedName);..$nested;_rc.closeNode()"
        case applySelectSymbolOpsFun(name, "$div$eq", value :: Nil) =>
          q"_rc.setAttr(${treeToKebabCase.convert(name)}, $value)"
        case Apply(selectDsl("text"), text :: Nil) =>
          q"_rc.addTextNode($text)"
//        case _ if tree.tpe <:< programmaticTemplateType => q"$tree.run()"
//        case _ if tree.tpe <:< documentType =>
//          val transformedTree = super.transform(tree)
//          q"$transformedTree.toProgrammaticTemplate(_rc).run()"
//        case _ if tree.tpe <:< documentType => q"""
//          $tree match {
//            case __x: $programmaticTemplateType => __x.run()
//            case __x: $documentType => __x.toProgrammaticTemplate(_rc).run()
//          }
//          """
        case _ => super.transform(tree)
      }
    }

//    def rec(tree: c.Tree): c.Tree = {
//      tree collect {
//        case applySelectSymbolOpsFun(name, "apply", xs) =>
//          val nested = xs.map(rec)
//          val kebabedName = treeToKebabCase.convert(name)
//          q"_rc.openNode($kebabedName);..$nested;_rc.closeNode()"
//        case _ => tree
//      }
//    }
    val xx = c.untypecheck(transformer.transform(document))
    val tc = q"$prefix.templateContext"
    val yy = q"$tc.ProgrammaticTemplate({ val _rc = $tc.RenderContext.Dummy;$xx })"
    println(yy)
    c.typecheck(yy)
  }
//  def toProgrammaticTemplate(document: c.Tree): c.Tree = c.inferImplicitValue(typeOf[RenderContext[String]]) match {
//    case EmptyTree =>
//      val pos = c.enclosingPosition
//      c.error(pos, "You should have implicit korolev.template.RenderContext in the scope")
//    case userRc =>
//      def aux(tree: c.Tree): TemplateMacro[c.Tree] = tree match {
//        case q"""korolev.template.dsl.<>""" => Empty
//        case q"""korolev.template.dsl.unit(())""" => Empty
//        case q"""korolev.template.dsl.text($text)""" => Text(text)
//        case q"""korolev.template.dsl.seq(..$xs)""" => Fragment(xs.map(aux))
//        case q"""korolev.template.dsl.SymbolOps(scala.Symbol.apply($name))./=($value)""" => Attr(name, value)
//        case q"""korolev.template.dsl.SymbolOps(scala.Symbol.apply($name)).apply(..$xs)""" => Node(name, xs.map(aux))
//        case _ if tree.tpe == typeOf[Misc[String]] => CompileTimeMisc(tree)
//        case _ => println(s"Unknown: $tree"); Empty
//      }
//      // A render context which create a code
//      // for each call method of itself.
//      // Code is saved in buffer
//      object compileTimeRc extends RenderContext[c.Tree] {
//        val buffer = mutable.Buffer.empty[c.Tree]
//        def openNode(name: c.Tree): Unit = buffer += q"$userRc.openNode($name)"
//        def closeNode(): Unit = buffer += q"$userRc.closeNode()"
//        def setAttr(name: c.Tree, value: c.Tree): Unit = buffer += q"$userRc.setAttr($name, $value)"
//        def addTextNode(text: c.Tree): Unit = buffer += q"$userRc.addTextNode($text)"
//        def addMisc(misc: Misc[c.Tree]): Unit = misc match {
//          case CompileTimeMisc(tree) => buffer += q"$userRc.addMisc($tree)"
//        }
//      }
//      aux(document)
//        .toProgrammaticTemplate(compileTimeRc)
//        .run()
//      q"""
//        korolev.template.ProgrammaticTemplate[String] {
//          ..${compileTimeRc.buffer}
//        }
//      """
//  }
  trait Extractor[A, B] {
    def unapply(arg: A): Option[B]
  }

  object extractor {
    def apply[B](f: PartialFunction[c.Tree, B]): Extractor[c.Tree, B] = new Extractor[c.Tree, B] {
      val liftedF = f.lift
      def unapply(value: c.Tree): Option[B] =
        liftedF(value)
    }
  }

  trait Checker { def unapply(arg: c.Tree): Boolean }

  object checker {
    def apply(f: PartialFunction[c.Tree, Any]): Checker = new Checker {
      def unapply(value: c.Tree): Boolean = f.isDefinedAt(value)
    }
  }

}

