package benchmark

import munit.*
import io.equiv.panproc.hpfl.*

class ParSeq extends munit.FunSuite:
  test("parallel vs sequential") {
    val params = List(Benchmark.LTSParams(5, 2, 10), Benchmark.LTSParams(5, 3, 15), Benchmark.LTSParams(7, 2, 20))
    val algorithms = List("seq", "par")
    val formulae = List("ready trace", "trace")

    val out_path = "benchmarks/par_seq.csv"

    val problems = for
      algorithm <- algorithms
      formula <- formulae
      param <- params
    yield Benchmark.Problem(
      param,
      formula,
      algorithm
    )
    val runner = Benchmark.Runner(problems, nInstances = 500, nRuns = 5)
    val results = runner.run()
    Benchmark.resultsToCSV(out_path, results)
  }