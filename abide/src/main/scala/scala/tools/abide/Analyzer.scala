package scala.tools.abide

import scala.tools.nsc._

/**
 * Supertrait for analyzer generator objects.
 *
 * Each [[Rule]] must be assigned an [[AnalyzerGenerator]] that will instantiate the [[Analyzer]] necessary
 * for rule application. One [[Analyzer]] will typically apply multiple rules.
 *
 * Sometimes, a new [[Analyzer]] type can actually apply the rules from other pre-existing
 * [[Analyzer]] types in a more optimal or general way. To enable such extension points, the
 * [[AnalyzerGenerator.subsumes]] field describes which generators can be skipped once this
 * one has been created.
 *
 * @see [[traversal.FusingTraversalAnalyzerGenerator]] for a concrete example
 * @see [[Analyzer]]
 */
trait AnalyzerGenerator {

  /**
   * Builds a new [[Analyzer]] instance based on a compiler (scala.reflect.internal.SymbolTable), and
   * a list of rules. The [[Analyzer thus generated will then apply these rules to provided trees.
   */
  def apply(_global: Global)(rules: List[Rule]): List[Analyzer{val global: _global.type}]

  /**
   * Subsumption mechanism that enables optimized or generalized analyzers to replace simpler ones.
   * In order to subsume (ie. replace) a given analyzer, simply add its generator to the subsumption set.
   */
  def subsumes: Set[AnalyzerGenerator]
}

/**
 * Supertrait for classes that apply [[Rule]]s.
 *
 * In many cases, rules can be grouped together in a logical way to optimize tree traversal, or keep the
 * traversal logic outside of the rules. This logic should be contained inside the [[Analyzer]] class
 * that will apply it's contained rules to provided trees.
 *
 * @see [[traversal.FusingTraversalAnalyzer]] for a concrete example
 * @see [[AnalyzerGenerator]]
 */
trait Analyzer {
  val global: Global
  import global._

  /** Applies the rules contained in this [[Analyzer]] to the provided tree and return a list of new warnings. */
  def apply(tree: Tree): List[Warning]
}
