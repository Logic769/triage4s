package triage4s.demo

import java.util.concurrent.atomic.AtomicLong
import org.llm4s.llmconnect.LLMClient
import org.llm4s.llmconnect.model.{Completion, CompletionOptions, Conversation, StreamedChunk}
import org.llm4s.reliability.{
  CircuitBreakerConfig,
  RateLimitConfig,
  ReliabilityConfig,
  ReliableClient,
  RetryPolicy
}
import org.llm4s.types.Result
import scala.concurrent.duration.*

/** Makes the demo's provider calls resilient with LLM4S's `ReliableClient` (#91): a circuit breaker that
  * stops a batch from hammering a provider that keeps failing, a deadline per call, an optional local rate
  * limit and - for providers whose SDK does not already retry - retries of transient errors with exponential
  * backoff.
  *
  * The OpenAI, Azure and Anthropic clients run on vendor SDKs that retry a transient failure up to 2 times by
  * themselves; retrying again here would multiply the attempts (3 x 3), so for them `ReliableClient` adds no
  * retries. Authentication, validation and other non-transient errors are never retried. Malformed model
  * output is not a provider error at all: Triage4s's own validation reports it.
  */
object Reliability:

  /** Providers whose client already retries transient failures inside its SDK. */
  val SdkRetrying: Set[String] = Set("openai", "azure", "anthropic")

  /** How long one classification call may take, its retries included, before it fails. */
  val Deadline: FiniteDuration = 3.minutes

  /** The reliability settings for `provider`; `requestsPerMinute`, when set, turns on a local rate limit. */
  def config(provider: String, requestsPerMinute: Option[Int]): ReliabilityConfig =
    ReliabilityConfig(
      retryPolicy =
        if SdkRetrying.contains(provider) then RetryPolicy.noRetry
        else RetryPolicy.exponentialBackoff(maxAttempts = 3, baseDelay = 1.second, maxDelay = 8.seconds),
      circuitBreaker = CircuitBreakerConfig(failureThreshold = 3, recoveryTimeout = 30.seconds),
      rateLimit = requestsPerMinute.fold(RateLimitConfig.disabled)(rpm =>
        RateLimitConfig(enabled = true, requestsPerMinute = rpm, burstCapacity = rpm)
      ),
      deadline = Some(Deadline)
    )

  /** `configured`, its client wrapped in a `ReliableClient`, and a counter of the calls that reached the
    * provider - retries included - so the run summary can say when an issue needed more than one attempt.
    * `sleep` is how the client waits between attempts; tests pass one that does not.
    */
  def wrap(
      configured: ConfiguredProvider,
      requestsPerMinute: Option[Int],
      sleep: FiniteDuration => Unit = delay => Thread.sleep(delay.toMillis)
  ): (ConfiguredProvider, AttemptCounter) =
    val counter = new AttemptCounter(configured.client)
    val reliable = new ReliableClient(
      counter,
      configured.provider,
      config(configured.provider, requestsPerMinute),
      sleep = sleep
    )
    (configured.copy(client = reliable), counter)

/** Counts the calls that reach the wrapped client: one per attempt. */
final class AttemptCounter(underlying: LLMClient) extends LLMClient:
  private val count = new AtomicLong(0)

  /** Calls so far. */
  def attempts: Long = count.get

  override def complete(conversation: Conversation, options: CompletionOptions): Result[Completion] =
    count.incrementAndGet()
    underlying.complete(conversation, options)

  override def streamComplete(
      conversation: Conversation,
      options: CompletionOptions,
      onChunk: StreamedChunk => Unit
  ): Result[Completion] =
    count.incrementAndGet()
    underlying.streamComplete(conversation, options, onChunk)

  override def getContextWindow(): Int = underlying.getContextWindow()
  override def getReserveCompletion(): Int = underlying.getReserveCompletion()
  override def close(): Unit = underlying.close()
