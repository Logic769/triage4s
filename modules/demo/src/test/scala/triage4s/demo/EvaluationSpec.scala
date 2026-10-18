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

  test("lists each mismatched example by ID, and missing results separately"):
    def recorded(id: String, issueType: Option[String], component: String, review: Boolean) =
      val in = TriageInput(id, "CLI", "Unclear request")
      val result = TriageResult(
        issueType,
        Some(component),
        "Recorded for scoring.",
        Vector(Evidence("component", "title", "CLI")) ++ issueType.map(_ =>
          Evidence("issueType", "title", "CLI")
        ),
        review,
        if review then Vector("Check.") else Vector.empty
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
      recorded("ok", Some("bug"), "cli", false),
      recorded("type", Some("bug"), "cli", false),
      recorded("component", Some("bug"), "storage", false),
      recorded("review", Some("bug"), "cli", true)
    )
    val hashOf = items.map(i => i.issue.input.id -> i.issue.hash).toMap
    def annotation(id: String, types: Vector[Option[String]]) =
      Expected(
        id,
        hashOf.getOrElse(id, "missing"),
        types,
        Vector(Some("cli")),
        false,
        "heldout",
        "test reviewer"
      )
    val annotations = Vector(
      annotation("review", Vector(Some("bug"))),
      annotation("type", Vector(Some("enhancement"), None)),
      annotation("ok", Vector(Some("bug"))),
      annotation("component", Vector(Some("bug"))),
      annotation("gone", Vector(Some("bug")))
    )
    val report = Evaluation.score(replay.copy(items = items), annotations, "heldout").toOption.get
    val mismatches = report("mismatches").arr
    assert(mismatches.map(_("id").str) == Seq("component", "review", "type"))
    assert(
      mismatches
        .map(_("failed").arr.map(_.str).toSeq) == Seq(Seq("component"), Seq("needsReview"), Seq("issueType"))
    )
    val typeRow = mismatches.find(_("id").str == "type").get
    assert(typeRow("predicted")("issueType").str == "bug")
    assert(typeRow("expected")("acceptedTypes").arr.toSeq == Seq(ujson.Str("enhancement"), ujson.Null))
    assert(typeRow("expected")("needsReview").bool == false)
    assert(report("missing").arr.map(_.str) == Seq("gone"))
    assert(!report.render().contains("Unclear request"))
