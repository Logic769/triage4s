package triage4s.demo

import org.llm4s.llmconnect.LLMClient
import org.llm4s.llmconnect.config.{ContextWindowResolver, OpenAIConfig, ProviderTimeouts}
import org.llm4s.llmconnect.provider.OpenAIClient
import org.llm4s.model.ModelRegistryService
import org.llm4s.types.Result
import scala.concurrent.duration.DurationInt
import scala.util.Try

final class AppConfig(
    val port: Int,
    val model: String,
    val apiKey: Option[String],
    val githubToken: Option[String]
):
  override def toString: String = s"AppConfig(port=$port, model=$model, credentials=[redacted])"

object AppConfig:
  /** The OpenAI endpoint the demo calls; always HTTPS, so the key never travels in plaintext. */
  val OpenAIBaseUrl = "https://api.openai.com/v1"

  /** How long one attempt of a classification call may take before it fails. */
  val RequestTimeout = 60.seconds

  val TransportNotice =
    "No application retries; the OpenAI SDK under LLM4S retries a transient failure (rate limit, 5xx, timeout) " +
      "up to 2 times with backoff. Each attempt times out after 60s. Output capped at 1200 tokens."

  def load(environment: Map[String, String]): Either[String, AppConfig] =
    def optional(key: String) = environment.get(key).map(_.trim).filter(_.nonEmpty)
    val port = optional("TRIAGE_PORT").getOrElse("8080")
    val model = optional("TRIAGE_MODEL").getOrElse("gpt-4o-mini")
    for
      parsed <- port.toIntOption
        .filter(p => p >= 1024 && p <= 65535)
        .toRight("TRIAGE_PORT must be an integer from 1024 to 65535.")
      _ <- Either.cond(model.matches("[a-zA-Z0-9._:-]{1,100}"), (), "TRIAGE_MODEL has an invalid format.")
    yield new AppConfig(parsed, model, optional("OPENAI_API_KEY"), optional("GITHUB_TOKEN"))

  /** The LLM4S provider configuration for `config`'s model and `key`: the OpenAI endpoint, with
    * [[RequestTimeout]].
    */
  def providerConfig(config: AppConfig, key: String)(using ContextWindowResolver): Result[OpenAIConfig] =
    OpenAIConfig
      .fromValues(config.model, key, None, OpenAIBaseUrl)
      .map(_.withTimeouts(ProviderTimeouts(request = Some(RequestTimeout))))

  def client(config: AppConfig): Either[String, LLMClient] =
    for
      key <- config.apiKey.toRight(
        "OPENAI_API_KEY is not configured. Set it locally and restart; do not paste keys into chat."
      )
      registry <- ModelRegistryService.default().left.map(_ => "Failed to load bundled LLM4S model metadata.")
      client <- Try {
        given ModelRegistryService = registry
        given ContextWindowResolver = ContextWindowResolver(registry)
        providerConfig(config, key).flatMap(OpenAIClient(_))
      }.toEither.left
        .map(e => s"Could not configure OpenAI (${e.getClass.getSimpleName}).")
        .flatMap(_.left.map(_ => "LLM4S could not create the OpenAI client."))
    yield client
