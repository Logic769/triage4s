package triage4s.javaapi

import org.llm4s.config.Llm4sConfig
import org.llm4s.llmconnect.{LLMClient, LLMConnect}
import scala.util.Try
import triage4s.{Taxonomy, Triage, TriageError, TriageInput}

/** Evidence-backed issue triage from Java (#97). Every signature uses Java types, and no call throws: a
  * failure is a [[JResult]] with its [[JErrorKind]].
  *
  * {{{
  * try (JTriage triage = JTriage.fromConfiguration("openai-main").get()) {
  *     JResult<JTriageResponse> result = triage.triage("42", "CLI crashes", "The launcher crashes on startup.");
  *     result.get().issueType().ifPresent(System.out::println);
  * }
  * }}}
  *
  * Built with [[JTriage.of]], it uses a client you own and does not close it; built with
  * [[JTriage.fromConfiguration]], it creates the client and `close()` closes it.
  */
final class JTriage private (client: LLMClient, taxonomy: Taxonomy, owned: Boolean) extends AutoCloseable:
  private val triage = new Triage(client)

  /** Suggests a type and component for the issue `id` with `title` and `body`. */
  def triage(id: String, title: String, body: String): JResult[JTriageResponse] =
    if id == null || title == null || body == null then
      JResult.failure(JErrorKind.INVALID_INPUT, "id, title and body must not be null")
    else
      triage.triage(TriageInput(id, title, body), taxonomy) match
        case Right(response) => JResult.success(new JTriageResponse(response))
        case Left(error)     => JResult.failure(JTriage.kindOf(error), error.message)

  /** Closes the client when this `JTriage` created it ([[JTriage.fromConfiguration]]); otherwise does
    * nothing.
    */
  override def close(): Unit = if owned then client.close()

object JTriage:
  /** Triage with `client` and the default taxonomy. The client stays yours to close. */
  def of(client: LLMClient): JTriage = of(client, JTaxonomy.defaultTaxonomy())

  /** Triage with `client` and `taxonomy`. The client stays yours to close. */
  def of(client: LLMClient, taxonomy: JTaxonomy): JTriage =
    new JTriage(
      java.util.Objects.requireNonNull(client, "client"),
      java.util.Objects.requireNonNull(taxonomy, "taxonomy").taxonomy,
      owned = false
    )

  /** Triage with the client of the LLM4S provider section `section` (in your `application.conf`, e.g.
    * `llm4s.providers.openai-main`) and the default taxonomy. The section's provider module, such as
    * `llm4s-openai`, must be on the classpath. A failure to load or build it is a
    * [[JErrorKind.CONFIGURATION]] result.
    */
  def fromConfiguration(section: String): JResult[JTriage] =
    val built = for
      name <- Option(section).filter(_.trim.nonEmpty).toRight("the provider section must not be blank")
      registry <- Llm4sConfig.modelRegistryService().left.map(_.message)
      provider <- Llm4sConfig.provider(name).left.map(_.message)
      client <- Try(LLMConnect.getClient(provider)(using registry)).toEither.left
        .map(e => s"${e.getClass.getSimpleName}: ${e.getMessage}")
        .flatMap(_.left.map(_.message))
    yield new JTriage(client, Taxonomy.copilotLens, owned = true)
    built.fold(JResult.failure(JErrorKind.CONFIGURATION, _), JResult.success)

  private def kindOf(error: TriageError): JErrorKind = error match
    case TriageError.InvalidInput(_)    => JErrorKind.INVALID_INPUT
    case TriageError.InvalidTaxonomy(_) => JErrorKind.INVALID_TAXONOMY
    case TriageError.InvalidOutput(_)   => JErrorKind.INVALID_OUTPUT
    case TriageError.ProviderFailure(_) => JErrorKind.PROVIDER_FAILURE
