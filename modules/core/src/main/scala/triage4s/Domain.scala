package triage4s

import upickle.default.*

final case class TriageInput(id: String, title: String, body: String) derives ReadWriter
final case class Category(id: String, description: String) derives ReadWriter
final case class Taxonomy(version: String, types: Vector[Category], components: Vector[Category])
    derives ReadWriter
final case class Evidence(target: String, field: String, quote: String) derives ReadWriter
final case class TriageResult(
    issueType: Option[String],
    component: Option[String],
    rationale: String,
    evidence: Vector[Evidence],
    needsReview: Boolean,
    reviewReasons: Vector[String]
) derives ReadWriter
final case class TokenUsage(prompt: Int, completion: Int, total: Int) derives ReadWriter
final case class TriageResponse(
    result: TriageResult,
    model: String,
    usage: Option[TokenUsage]
) derives ReadWriter

enum TriageError(val message: String):
  case InvalidInput(detail: String) extends TriageError(detail)
  case InvalidTaxonomy(detail: String) extends TriageError(detail)
  case InvalidOutput(detail: String) extends TriageError(detail)
  case ProviderFailure(detail: String) extends TriageError(detail)

trait Classifier:
  def triage(input: TriageInput, taxonomy: Taxonomy): Either[TriageError, TriageResponse]

  /** Classifies a batch with one taxonomy. An invalid taxonomy or a repeated input ID fails the whole batch
    * before any issue is classified; otherwise every input is classified in order and keeps its own result,
    * so one failure does not stop the rest.
    */
  def triageAll(
      inputs: Seq[TriageInput],
      taxonomy: Taxonomy
  ): Either[TriageError, Vector[(String, Either[TriageError, TriageResponse])]] =
    Classifier.checkBatch(inputs, taxonomy).map(_ => inputs.toVector.map(i => i.id -> triage(i, taxonomy)))

object Classifier:
  private[triage4s] def checkBatch(inputs: Seq[TriageInput], taxonomy: Taxonomy): Either[TriageError, Unit] =
    Validation.validateTaxonomy(taxonomy).flatMap { _ =>
      val repeated = inputs.groupBy(_.id).collect { case (id, group) if group.size > 1 => id }.toVector.sorted
      Either.cond(
        repeated.isEmpty,
        (),
        TriageError
          .InvalidInput(s"Input IDs must be unique within a batch; repeated: ${repeated.mkString(", ")}.")
      )
    }

object Taxonomy:
  val copilotLens: Taxonomy = Taxonomy(
    "copilot-lens-v1",
    Vector(
      Category("bug", "Existing behavior is incorrect, broken, or loses data."),
      Category("enhancement", "Add or improve a capability without evidence of broken existing behavior."),
      Category("documentation", "Change user-facing documentation or examples."),
      Category("question", "Request an explanation or guidance rather than a software change.")
    ),
    Vector(
      Category("cli", "Command-line arguments, launching the browser, and local server startup."),
      Category("parsing", "Reading and interpreting assistant session files, JSONL, or imported data."),
      Category("analytics", "Usage statistics, costs, metrics, reporting, and analytics API endpoints."),
      Category("storage", "Database access, caches, persistence, and file storage."),
      Category("documentation", "README, guides, and user-facing examples.")
    )
  )
