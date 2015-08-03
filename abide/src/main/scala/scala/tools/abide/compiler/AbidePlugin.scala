package scala.tools.abide.compiler

import scala.tools.nsc._
import scala.tools.nsc.plugins._
import scala.reflect.runtime.{ universe => ru }

import scala.tools.abide._
import scala.tools.abide.presentation._

/**
 * AbidePlugin
 *
 * Compiler plugin for running the Abide framework. The plugin runs right after `typer` and uses
 * a series of rules and analyzers to perform actual verification. It then uses a [[presentation.Presenter]] to
 * output the result of the verification.
 *
 * The plugin accepts a series of abide-specific arguments (use as -P:abide:argName:argValue) :
 *
 *  - ruleClass :
 *      Specifies the full name of each rule that should be handled by the plugin (eg. com.typesafe.abide.samples.UnusedMember)
 *      This option can (and should) appear multiple times in the arguments array to specify all rules we're dealing with.
 *
 *  - analyzerClass :
 *      Specifies the full name of all analyzer generator objects that can be used to instantiate rules. In practice, since the
 *      analyzer generator is actually a member of the rule, this argument can be omitted. However, the subsumption mechanism
 *      (see [[AnalyzerGenerator]]) enables users to provide more powerful analyzers that will replace the default analyzer
 *      statically specified in the class description. Such extension analyzers _must_ appear in an `analyzerClass` argument.
 *      As in ruleClass, analyzerClass can (and generally should) appear multiple times in the arguments array.
 *
 *  - abidecp :
 *      Abide can split up rule sets into multiple jars, so when using Abide in compiler plugin mode, these jars need to be
 *      specified to the compiler so it can actually load the rules. Therefore, we provide the `abidecp` option that must consist
 *      in a colon-separated list of classpath entries pointing to rule locations.
 *
 * Note that unlike analyzers, rule context cannot be overriden by arguments and must be specified in the rule's companion object,
 * since configurations are custom tailored to single rules (for more information, see [[ContextGenerator]]).
 *
 * @see [[scala.tools.abide.ContextGenerator]]
 * @see [[scala.tools.abide.AnalyzerGenerator]]
 */
class AbidePlugin(val global: Global) extends Plugin {
  import global._

  val name = "abide"
  val description = "static code analysis for Scala"

  val components: List[PluginComponent] = List(component)

  private lazy val classLoader = new java.net.URLClassLoader(
    abideCp.split(":").filter(_ != "").map(f => new java.io.File(f).toURI.toURL),
    getClass.getClassLoader
  )

  private lazy val mirror = ru.runtimeMirror(classLoader)
  private lazy val ruleMirrors = for (ruleClass <- ruleClasses) yield {
    val ruleSymbol = mirror staticClass ruleClass
    val ruleMirror = mirror reflectClass ruleSymbol

    ruleSymbol -> ruleMirror
  }

  private lazy val analyzerGenerators = for (analyzerClass <- analyzerClasses) yield {
    val analyzerSymbol = mirror staticModule analyzerClass
    val analyzerMirror = mirror reflectModule analyzerSymbol
    analyzerMirror.instance.asInstanceOf[AnalyzerGenerator]
  }

  private lazy val presenterGenerators = for (presenterClass <- presenterClasses) yield {
    val presenterSymbol = mirror staticModule presenterClass
    val presenterMirror = mirror reflectModule presenterSymbol
    presenterMirror.instance.asInstanceOf[PresenterGenerator]
  }

  private lazy val ruleContexts = {
    import ru._

    def contextGenerator(sym: ru.Symbol): ru.ModuleSymbol = sym.asClass.baseClasses.collectFirst {
      case tpe: ru.TypeSymbol if tpe.toType.companion <:< ru.typeOf[ContextGenerator] =>
        tpe.toType.companion.typeSymbol.asClass.module.asModule
    }.get

    val contextGenerators = for (rule @ (ruleSymbol, ruleMirror) <- ruleMirrors) yield {
      val generatorSymbol = contextGenerator(ruleSymbol)
      val generatorMirror = mirror reflectModule generatorSymbol

      rule -> generatorMirror.instance.asInstanceOf[ContextGenerator]
    }

    def typeTag[T: ru.TypeTag](obj: T): ru.TypeTag[T] = ru.typeTag[T]
    def generalize[T1 <: Context: ru.TypeTag, T2 <: Context: ru.TypeTag](o1: T1, o2: T2): Context = {
      if (typeTag(o1).tpe <:< typeTag(o2).tpe) o1 else o2
    }

    contextGenerators.foldLeft(List.empty[((ru.ClassSymbol, ru.ClassMirror), Context)]) {
      case (list, (rule @ (ruleSymbol, ruleMirror), generator)) =>
        val context = generator.getContext(global)
        val bottomCtx = list.foldLeft(context) { case (acc, (rule, ctx)) => generalize(acc, ctx) }
        (rule -> bottomCtx) :: (list map { case (rule, ctx) => rule -> generalize(ctx, bottomCtx) })
    }.toMap
  }

  private lazy val rules = for (rule @ (ruleSymbol, ruleMirror) <- ruleMirrors) yield {
    val constructorSymbol = ruleSymbol.typeSignature.member(ru.termNames.CONSTRUCTOR).asMethod
    val constructorMirror = ruleMirror reflectConstructor constructorSymbol
    val context = ruleContexts(rule)

    constructorMirror(context).asInstanceOf[Rule { val context: Context { val universe: AbidePlugin.this.global.type } }]
  }

  private lazy val analyzers: List[Analyzer{ val global: AbidePlugin.this.global.type }] = {
    analyzerGenerators.flatMap(_.apply(global)(rules))
  }

  private lazy val presenters = presenterGenerators.map(_.apply(global))

  private[abide] object component extends {
    val global: AbidePlugin.this.global.type = AbidePlugin.this.global
  } with PluginComponent {
    val runsAfter = List("typer")
    val phaseName = AbidePlugin.this.name

    def newPhase(prev: Phase) = new StdPhase(prev) {
      override def name = AbidePlugin.this.name

      def apply(unit: CompilationUnit): Unit = {
        val report = analyzers.flatMap(_.apply(unit.body))
        presenters.foreach(_.apply(unit, report))
      }
    }
  }

  private var abideCp: String = ""
  private var ruleClasses: List[String] = Nil
  private var analyzerClasses: List[String] = Nil
  private var presenterClasses: List[String] = Nil

  override def processOptions(options: List[String], error: String => Unit): Unit = {
    for (option <- options) {
      if (option.startsWith("ruleClass:")) {
        ruleClasses ::= option.substring("ruleClass:".length)
      }
      else if (option.startsWith("analyzerClass:")) {
        analyzerClasses ::= option.substring("analyzerClass:".length)
      }
      else if (option.startsWith("presenterClass:")) {
        presenterClasses ::= option.substring("presenterClass:".length)
      }
      else if (option.startsWith("abidecp:")) {
        abideCp = option.substring("abidecp:".length)
      }
      else {
        global.reporter.error(NoPosition, "Unexpected abide option: " + option)
      }
    }

    if (presenterClasses.isEmpty) {
      val defaultPresenter = ConsolePresenterGenerator
      val presenterPackage = defaultPresenter.getClass.getPackage.getName
      val presenterName = defaultPresenter.getClass.getName
      presenterClasses ::= s"$presenterPackage$presenterName"
    }
  }
}
