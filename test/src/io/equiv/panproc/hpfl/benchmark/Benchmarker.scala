package benchmark

import hpfltest.TestData
import io.equiv.panproc.ts.TransitionSystem
import io.equiv.panproc.hpfl.*
import java.io.FileWriter
import java.io.File

object Benchmark:
  val equivalences = Map(
    "trace" -> DefaultEquivalences.trace,
    "completed trace" -> DefaultEquivalences.completedTrace,
    "failures" -> DefaultEquivalences.failures,
    "failure trace" -> DefaultEquivalences.failureTrace,
    "readiness" -> DefaultEquivalences.readiness,
    "ready trace" -> DefaultEquivalences.readyTrace,
    "simulation" -> DefaultEquivalences.simulation,
    "completed simulation" -> DefaultEquivalences.completedSimulation,
    "ready simulation" -> DefaultEquivalences.readySimulation,
    "two nested simulation" -> DefaultEquivalences.twoNestedSimulation,
    "bisimulation" -> DefaultEquivalences.bisimulation
  )

  case class LTSParams(states: Int, actions: Int, transitions: Int)

  case class LTSPair(
      lts1: TransitionSystem[Int, Int, Int],
      lts2: TransitionSystem[Int, Int, Int]
  )

  def generateLTS(params: LTSParams): TransitionSystem[Int, Int, Int] =
    var tuples: Set[(Int, Int, Int)] = Set()
    while tuples.size < params.transitions do
      val from = util.Random.nextInt(params.states)
      val action = util.Random.nextInt(params.actions)
      val to = util.Random.nextInt(params.states)
      tuples += ((from, action, to))
    TestData.makeTS(tuples.toSeq*)

  def generateDummy(actionCount: Int): TransitionSystem[Int, Int, Int] =
    TestData.makeTS(Seq.range(0, actionCount).map(i => (0, i, 0))*)

  def calcTiming(times: List[Long], keep_percentage: Double) =
    val median = times.sorted.apply(times.size / 2)
    val sort_by_dist = times.sortBy(t => math.abs(t - median))
    val without_outliers = sort_by_dist.slice(0, (times.size * keep_percentage).toInt)
    without_outliers.sum / without_outliers.size

  def resultsToCSV(path: String, results: Map[Problem, Array[Result]]): Unit =
    val base_path = path.split("/").dropRight(1).mkString("/")
    // make base path if it doesn't exist
    val base_dir = if base_path == "" then File(".") else File(base_path)
    if !base_dir.exists() then
      base_dir.mkdirs()

    val writer = FileWriter(path)
    writer.write(
      "states,actions,transitions,formula,algorithm,instance,time,table_size,table_iters,table_calcs\n"
    )
    results.foreach { case (problem, results) =>
      results.zipWithIndex.foreach { case (result, i) =>
        writer.write(
          s"${problem.params.states},${problem.params.actions},${problem.params.transitions},\"${problem.formula}\",${problem.algorithm},$i,${result.time},${result.table_size},${result.table_iters},${result.table_calcs}\n"
        )
      }
    }
    writer.close()

  case class Problem(params: LTSParams, formula: String, algorithm: String)
  case class Result(time: Long, table_size: Int, table_iters: Int, table_calcs: Int)

  class Runner(
      problems: Iterable[Problem],
      nRuns: Int = 10,
      nInstances: Int = 100,
      keep: Double = 0.8
  ):
    // precompute checked formulae as typechecking is slow
    val checked_formulae: Map[(Int, String), HPFLCore.HPFLCore[Int, Char, HPFLType]] =
      // every used combination of actions and formulae needs to be instantiated
      // and typechecked
      val keys = problems.map(p => (p.params.actions, p.formula)).toSet
      keys.map(key =>
        val dummy = generateDummy(key._1)
        val checked =
          for
            instance <- Benchmark.equivalences(key._2).instantiate(List(dummy, dummy))
            checked <- typecheck(instance)
          yield checked
        assert(checked.isDefined)
        key -> checked.get
      ).toMap

    val instances: Map[LTSParams, Array[LTSPair]] =
      val params = problems.map(_.params).toSet
      params.map(p => p -> Array.fill(nInstances) { LTSPair(generateLTS(p), generateLTS(p)) }).toMap

    def run(): Map[Problem, Array[Result]] =
      var timings: Map[Problem, Array[List[Long]]] =
        problems.map(p => p -> Array.fill(nInstances) { List[Long]() }).toMap

      var solve_info: Map[Problem, Array[(Int, Int, Int)]] =
        problems.map(p => p -> Array.fill(nInstances) { (0, 0, 0) }).toMap

      var order: Seq[(Problem, Int)] =
        problems.flatMap(p => (0 until nInstances).map(i => (p, i))).toSeq

      def meanTableStats(stats: List[(Int, Int)]) =
        val size_mean: Int =
          if stats.size == 0
          then 0
          else
            stats.map(_._1).sum / stats.size
        val iters_mean: Int =
          if stats.size == 0
          then 0
          else
            stats.map(_._2).sum / stats.size
        val sum_calcs: Int = stats.map((s, i) => s * i).sum
        (size_mean, iters_mean, sum_calcs)

      for run_i <- 1 to nRuns do
        println(s"Run $run_i")
        order = util.Random.shuffle(order)
        for (problem, i) <- order do
          val pair = instances(problem.params)(i)
          val formula = checked_formulae((problem.params.actions, problem.formula))
          if problem.algorithm == "seq" then
            val solver = SeqSolver[Int, Int, Char, Int](List(pair.lts1, pair.lts2))
            val start = System.nanoTime()
            solver.solve(formula, List(), Map(), Map())
            val end = System.nanoTime()
            timings(problem)(i) = (end - start) :: timings(problem)(i)
            solve_info(problem)(i) = meanTableStats(solver.table_stats)
          else if problem.algorithm == "par" then
            val solver = ParSolver[Int, Int, Char, Int](List(pair.lts1, pair.lts2))
            val start = System.nanoTime()
            solver.solve(formula, List(), Map(), Map())
            val end = System.nanoTime()
            timings(problem)(i) = (end - start) :: timings(problem)(i)
            solve_info(problem)(i) = meanTableStats(solver.table_stats)
          else
            throw new IllegalArgumentException("Unknown algorithm")

      timings.map { case (p, ts) =>
        p -> ts.zipWithIndex.map { case (t, i) =>
          val time = calcTiming(t, keep)
          val info = solve_info(p)(i)
          Result(time, info._1, info._2, info._3)
        }
      }
