package triage4s.demo

import org.scalatest.funsuite.AnyFunSuite
import triage4s.*
import java.time.Instant

class EvaluationSpec extends AnyFunSuite:
  private val input = TriageInput("1", "CLI", "Unclear request")
  private val issue = Issue(
    input,
    "https://github.com/pavanvamsi3/copilot-lens/issues/1",
    "open",
    Vector.empty,
    Instant.now().toString,
    Issue.hashInput(input)
  )
  private val result = TriageResult(
    None,
    Some("cli"),
    "Type is unclear.",
    Vector(Evidence("component", "title", "CLI")),
    true,
    Vector("Insufficient context.")
  )
  private val replay = Replay(
    1,
    Instant.now().toString,
    Taxonomy.copilotLens,
    Triage.PromptVersion,
    Vector(RecordedIssue(issue, TriageResponse(result, "test-only", None)))
  )
  private val expected =
    Expected("1", issue.hash, Vector(None), Vector(Some("cli")), true, "heldout", "test reviewer")

  test("reports abstentions and missing outcomes in denominators"):
    val report = Evaluation.score(replay, Vector(expected, expected.copy(id = "2")), "heldout").toOption.get
    assert(report("expected").num == 2)
    assert(report("missingResults").num == 1)
    assert(report("typeAccuracy")("fraction").num == 0.5)
    assert(report("abstentions")("count").num == 1)
    assert(report("coverage")("fraction").num == 0.5)

  test("requires human annotation, nonempty evaluation split and matching source hashes"):
    assert(Evaluation.score(replay, Vector(expected.copy(reviewedBy = "")), "heldout").isLeft)
    assert(Evaluation.score(replay, Vector(expected.copy(inputHash = "wrong")), "heldout").isLeft)
    assert(Evaluation.score(replay, Vector(expected), "development").isLeft)
    assert(Evaluation.score(replay, Vector(expected, expected), "heldout").isLeft)

  test("reports precision and recall for every category, with null for an empty denominator"):
    def recorded(id: String, issueType: Option[String], component: String) =
      val in = TriageInput(id, "CLI", "Unclear request")
      val result = TriageResult(
        issueType,
        Some(component),
        "Recorded for scoring.",
        Vector(Evidence("component", "title", "CLI")) ++ issueType.map(_ =>
          Evidence("issueType", "title", "CLI")
        ),
        issueType.isEmpty,
        if issueType.isEmpty then Vector("Type unclear.") else Vector.empty
      )
      val is = Issue(
        in,
        s"https://github.com/pavanvamsi3/copilot-lens/issues/$id",
        "open",
        Vector.empty,
        Instant.now().toString,
        Issue.hashInput(in)
      )
      RecordedIssue(is, TriageResponse(result, "test-only", None))
    val items = Vector(
      recorded("a", Some("bug"), "cli"),
      recorded("b", Some("bug"), "storage"),
      recorded("c", None, "parsing")
    )
    val hashOf = items.map(i => i.issue.input.id -> i.issue.hash).toMap
    def annotation(id: String, types: Vector[Option[String]], components: Vector[Option[String]]) =
      Expected(id, hashOf.getOrElse(id, "missing"), types, components, false, "heldout", "test reviewer")
    val annotations = Vector(
      annotation("a", Vector(Some("bug")), Vector(Some("cli"))),
      annotation("b", Vector(Some("enhancement")), Vector(Some("storage"), Some("analytics"))),
      annotation("c", Vector(Some("question"), None), Vector(Some("parsing"))),
      annotation("d", Vector(Some("bug")), Vector(Some("cli")))
    )
    val report = Evaluation.score(replay.copy(items = items), annotations, "heldout").toOption.get
    val types = report("perCategory")("types")
    val components = report("perCategory")("components")
    def check(
        v: ujson.Value,
        predicted: Int,
        correct: Int,
        annotated: Int,
        precision: Option[Double],
        recall: Option[Double]
    ) =
      assert(
        v("predicted").num == predicted && v("correct").num == correct && v("annotated").num == annotated,
        v
      )
      assert(precision.fold(v("precision").isNull)(p => v("precision").num == p), v)
      assert(recall.fold(v("recall").isNull)(r => v("recall").num == r), v)
    assert(types.obj.keySet == Taxonomy.copilotLens.types.map(_.id).toSet)
    assert(components.obj.keySet == Taxonomy.copilotLens.components.map(_.id).toSet)
    check(types("bug"), 2, 1, 2, Some(0.5), Some(0.5))
    check(types("enhancement"), 0, 0, 1, None, Some(0.0))
    check(types("question"), 0, 0, 1, None, Some(0.0))
    check(types("documentation"), 0, 0, 0, None, None)
    check(components("cli"), 1, 1, 2, Some(1.0), Some(0.5))
    check(components("storage"), 1, 1, 1, Some(1.0), Some(1.0))
    check(components("analytics"), 0, 0, 1, None, Some(0.0))
    check(components("parsing"), 1, 1, 1, Some(1.0), Some(1.0))
    assert(report("expected").num == 4 && report("missingResults").num == 1)
