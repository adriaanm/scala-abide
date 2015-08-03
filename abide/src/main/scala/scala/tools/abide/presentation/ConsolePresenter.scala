package scala.tools.abide.presentation

import scala.tools.nsc._
import scala.tools.abide._

/**
 * ConsolePresenterGenerator
 *
 * @see [[ConsolePresenter]]
 */
object ConsolePresenterGenerator extends PresenterGenerator {
  def apply(_global: Global) = new ConsolePresenter { val global: _global.type = _global }
}

/**
 * ConsolePresenter
 *
 * Simple [[Presenter]] that outputs warnings as compiler warnings
 */
abstract class ConsolePresenter extends Presenter {
  import global._

  /** Outputs Abide warnings as compiler warnings */
  def apply(unit: CompilationUnit, warnings: List[Warning]): Unit = {
    warnings.foreach { warning =>
      global.warning(warning.pos, warning.message)
    }
  }
}
