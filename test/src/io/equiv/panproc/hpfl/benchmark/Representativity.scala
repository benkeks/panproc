package benchmark
import munit.*
import hpfltest.*

class Representativity extends munit.FunSuite:
  test("representativity") {
    val state_count = 6
    val action_count = 3
    val transition_count = 8
    val algorithm = "par"
    val formulae = Benchmark.equivalences.keys.toList

    val out_path = "benchmarks/equivalences.csv"

    val problems = for
      formula <- formulae
    yield Benchmark.Problem(
      Benchmark.LTSParams(state_count, action_count, transition_count),
      formula,
      algorithm
    )
    val t3 = Benchmark.LTSPair(TestData.makeTS(
        (1, 0, 2),
        (1, 0, 5),
        (2, 1, 3),
        (2, 2, 4),
        (3, 1, 4),
        (5, 0, 4),
        (5, 1, 6),
        (6, 0, 4)
      ),
      TestData.makeTS(
        (1, 0, 2),
        (1, 0, 5),
        (2, 1, 3),
        (2, 2, 4),
        (3, 0, 4),
        (5, 0, 4),
        (5, 1, 6),
        (6, 1, 4)
      ))
    val runner = Benchmark.Runner(problems, nInstances = 1000)
    runner.instances.foreach((k, v) => v(999) = t3)
    val results = runner.run()
    Benchmark.resultsToCSV(out_path, results)
  }