# Complexity Analyzer for Monotone Prolog

A semi-automatic, symbolic evaluation graph based complexity analyzer for monotone Prolog programs. Generates term rewrite systems that can be analyzed with tools like [TcT](http://cl-informatik.uibk.ac.at/software/tct/).

<br>

## Gitpod

Click the button below to start a new development environment:

[![Open in Gitpod](https://gitpod.io/button/open-in-gitpod.svg)](https://gitpod.io/#https://github.com/YtGz/Analyzing-the-Complexity-of-Monotone-Prolog)

<br>

## Installation

```bash
$ git clone https://github.com/YtGz/Analyzing-the-Complexity-of-Monotone-Prolog.git
$ cd Analyzing-the-Complexity-of-Monotone-Prolog/
$ mkdir lib
# monad-supply is not cloned yet, so we need to ignore the cabal.project file initially
$ cabal update --ignore-project
$ (cd lib && git clone https://github.com/YtGz/monad-supply.git)
# use hpack to generate monad-supply.cabal
$ (cd lib/monad-supply && cabal install --ignore-project hpack && hpack)
# fetch parsec-prolog library
$ (cd lib && git clone https://github.com/acharal/parsec-prolog.git)
# path of op.pl file is hardcoded in the parsec-prolog library
$ sudo mkdir -p /home/angel/edu/phd/code/parsec-prolog/pl && sudo cp lib/parsec-prolog/pl/op.pl /home/angel/edu/phd/code/parsec-prolog/pl/op.pl
# build camp
$ cabal build

$ cabal exec camp -- -i foo.pl -g bar.svg -o baz.trs
```
Requires [Cabal (version 3 or higher)](https://www.haskell.org/cabal/download.html) and a Haskell 2010 compatible compiler (e.g. [GHC](https://www.haskell.org/ghc/)).  
See [supported platforms](https://ghc.haskell.org/trac/ghc/wiki/Platforms).


<br>

## Usage


### Command line arguments

- `-i FILE` | `--input=FILE` path to input file **(required)**
- `-g FILE.svg` | `--graph=FILE.svg` path to graph svg output (**required**, file may be overwritten if it already exists)
- `-o FILE` | `--output=FILE` path to TRS output (**required**, file will be overwritten if it already exists)
- `-f` | `--force` force the creation of a vector graphic of the symbolic evaluation graph (default: only when required)
- `-h` | `--help` display the help


### Input file

Any **monotone** Prolog program. Analysis may fail in some instances.  
Syntax for queries:
```prolog
:- query.
```


### Output
In some instances the tool may ask you to look at the symbolic evaluation graph. In such a case a rendering of the graph is automatically generated at the path that was given as command line argument. The resulting vector file can be viewed with any modern browser. Graph rendering can be forced if inspection of the graph is desired.

Term rewrite systems are output in [TPDB legacy format](https://www.lri.fr/~marche/tpdb/format.html) and can directly be used as input for TcT. Tools for conversion to the newer format (XML) are [available](http://www.termination-portal.org/wiki/TPDB#Tools). No matter which tool you choose to use to analyze the rewrite systems make sure to determine complexity for *innermost rewriting*.


### Choose query class

The tool can reason over multiple queries simultaneously by grouping them into *query classes*.  
Query classes do not depend on the specific shape of query arguments but rather on them being ground (denoted 'In') or non-ground (denoted 'Out'). If the input file happens to contain multiple query classes, the tool will offer a choice before starting the analysis.


### Groundness analysis

Groundness analysis decides which argument positions will surely become ground for every possible answer substitution.  
This includes positions that were ground from the very beginning; positions are labeled by indices starting from `0`.  
When prompted by the tool, respond with a suitable `[Int]`.  
In theory this task is automatable: you can add your own groundness analyzer by replacing the [groundnessAnalysis](https://github.com/YtGz/Analyzing-the-Complexity-of-Monotone-Prolog/blob/f5e5d77af556c254ccc2f6893a269c15560a5d4b/src/SymbolicEvaluationGraphs/InferenceRules.hs#L254-L285) function.

### Multiplicative split nodes

Some nodes of the graph may be considered 'multiplicative'.  
At the moment this criterion has to be checked manually.  
(Automatation would require a reliable determinacy analysis.)  
Potential candidates are marked red and labeled by the node number.  
A split node is *multiplicative* iff:  
<ol type="A">
  <li>its first successor is not deterministic or</li>
  <li>
    <ol type="i">
      <li>it can reach itself via a non-empty path,</li>  
      <li>its first successor reaches a SUC node, and</li>  
      <li>this SUC node reaches a cycle in the graph</li>
    </ol>
  </li>
</ol>
(A query is deterministic iff it generates at most one answer substitution at most once.)  
When prompted by the tool, respond with a suitable `[Int]`, stating the node numbers of the multiplicative split nodes.


### Multiple term rewrite systems

The symbolic evaluation graph is split into subgraphs at multiplicative split nodes before the transformation to a term rewrite system, thus multiple rewrite systems may be generated. By looking at the labeling of multiplicative split nodes and function symbols in the TRS, each TRS can easily be matched to a subgraph starting at either the root node or a mult. split node. To calculate the final complexity bound each rewrite system has to be analyzed separately (e.g. with [TcT](http://cl-informatik.uibk.ac.at/software/tct/)) and the results have to be combined in the following way:
- bounds of the two subgraphs of a multiplicative split node have to be multiplied
- bound of the subgraph above a multiplicative split node has to be added


<br>
## Optimizing the heuristic

Several parameters control the heuristic used to construct the symbolic evaluation graph:
- **minExSteps:**  
Determines the number of CASE nodes that have to be traversed before the graph can be closed by an INSTANCE rule or a GENERALIZATION, PARALLEL or SPLIT inference rule can be applied.
- **maxBranchingFactor:**  
The branching factor of a function symbol f is the number of clauses h :- B in the Prolog program for which root(h) = f. This parameter determines how big the branching factor of a recursive (w.r.t. the Prolog program) function symbol f has to be such that a state with a clause h :- B with root(h) = f is still considered a suitable instance candidate even if it has more different variables than the state we want to draw the instance edge from.  
If this parameter is chosen too small too much precision is lost, while a high value can result in enormously big graphs.
- **finiteGeneralizationDepth ≥ 2:**  
Maximum nested depth of function symbols before a GENERALIZATION will be applied.  
Values too big lead to unfeasible graphs sizes, values that are too small diminish precision.
- **finiteGeneralizationPos ∈ {1,...,finiteGeneralizationDepth}:**  
Determines at what depth GENERALIZATIONS will be applied.  
Positions near the root make it easier to find instances later in the path (possibly resulting in a smaller graph), however, the more knowledge we keep about the shape of the generalized term, the more precision we preserve (GENERALIZATION generally is a precision impairing operation).  

All parameters can be found at the top of the file [Heuristic.hs](src/SymbolicEvaluationGraphs/Heuristic.hs).


<br>
## Further information

For further information see [J. Giesl, T. Ströder, P. Schneider-Kamp, F. Emmes, and C. Fuhs. Symbolic Evaluation Graphs and Term Rewriting: A General Methodology for Analyzing Logic Programs. In Proceedings of the 14th Symposium on Principles and Practice of Declarative Programming, PPDP ’12, pages 1–12, New York, NY, USA, 2012. ACM.](http://verify.rwth-aachen.de/giesl/papers/PPDP12.pdf)
