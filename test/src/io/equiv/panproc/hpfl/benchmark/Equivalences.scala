package benchmark
import munit.*
import hpfltest.*

class Equivalences extends munit.FunSuite:
  test("all equivalences") {
    val params = List(
      Benchmark.LTSParams(4, 2, 8),
      Benchmark.LTSParams(6, 2, 12),
      Benchmark.LTSParams(8, 2, 16),
      Benchmark.LTSParams(10, 2, 20),
      Benchmark.LTSParams(12, 2, 24),
      Benchmark.LTSParams(14, 2, 28)
    )
    val algorithm = "par"
    val formulae = Benchmark.equivalences.keys.toList

    val out_path = "benchmarks/equivalences2.csv"

    val problems =
      for
        formula <- formulae
        param <- params
      yield Benchmark.Problem(
        param,
        formula,
        algorithm
      )
    val runner = Benchmark.Runner(problems, nInstances = 100, nRuns = 5)
    val results = runner.run()
    Benchmark.resultsToCSV(out_path, results)
  }
