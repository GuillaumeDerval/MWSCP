package search

import oscar.algo.reversible.ReversibleInt
import oscar.algo.search.{Alternative, Branching}
import oscar.cp.core.variables.{CPBoolVar, CPSetVar}
import utils.{DoubleVar, MaximiseDouble}

class ImmediateSolutionSet(variables: Array[CPSetVar], objVar: DoubleVar, obj: MaximiseDouble, nextBranching: Branching) extends Branching {
  private[this] val context = variables(0).store
  private[this] val nVariables = variables.length
  private[this] val depthRev = new ReversibleInt(context, 0)
  private[this] var depth = 0

  override def alternatives(): Seq[Alternative] = {
    depth = depthRev.value

    // Update depth
    while (depth < nVariables && variables(depth).isBound) {
      depth += 1
    }

    if(depth != nVariables)
      depthRev.value = depth

    if(depth != nVariables && objVar.min > obj.best) {
      List(() => {
        context.doAndPropagate{
          variables.foreach(x => x.excludesAll())
        }
      }) ++ nextBranching.alternatives()
    }
    else
      nextBranching.alternatives()
  }
}

