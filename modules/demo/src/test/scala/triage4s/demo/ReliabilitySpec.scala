package triage4s.demo

import org.llm4s.error.{AuthenticationError, ServiceError}
import org.llm4s.llmconnect.model.{Conversation, UserMessage}
import org.llm4s.testkit.{Reply, ScriptedLLMClient}
import org.scalatest.funsuite.AnyFunSuite
import scala.collection.mutable.ArrayBuffer
import scala.concurrent.duration.*

/** Resilient provider calls through LLM4S's ReliableClient (#91); no test waits for real. */
class ReliabilitySpec extends AnyFunSuite:
  private val conversation = Conversation(Seq(UserMessage("classify")))
  private def unavailable = Reply.failure(ServiceError(503, "test", "unavailable"))

  private def wrapped(provider: String, replies: Reply*) =
    val sleeps = ArrayBuffer.empty[FiniteDuration]
    val client = ScriptedLLMClient.sequence(replies*)
    val (reliable, counter) =
      Reliability.wrap(ConfiguredProvider(client, provider, "m"), None, sleep = d => sleeps += d)
    (reliable.client, counter, sleeps)

  test("a transient error is retried with backoff for a provider whose SDK does not retry"):
    val (client, counter, sleeps) = wrapped("gemini", unavailable, unavailable, Reply.text("ok"))
    assert(client.complete(conversation).map(_.content) == Right("ok"))
    assert(counter.attempts == 3)
    assert(sleeps.size == 2)
    assert(sleeps(1) > sleeps(0))

  test("an authentication error is never retried"):
    val (client, counter, sleeps) =
      wrapped("gemini", Reply.failure(AuthenticationError("gemini", "bad key")), Reply.text("ok"))
    assert(client.complete(conversation).left.toOption.exists(_.isInstanceOf[AuthenticationError]))
    assert(counter.attempts == 1)
    assert(sleeps.isEmpty)

  test("no retries are added where the vendor SDK already retries"):
    Seq("openai", "anthropic").foreach { provider =>
      val (client, counter, _) = wrapped(provider, unavailable, Reply.text("ok"))
      assert(client.complete(conversation).isLeft)
      assert(counter.attempts == 1)
    }

  test("after three failed calls the circuit opens, and the next call fails without reaching the provider"):
    val (client, counter, _) = wrapped("openai", unavailable, unavailable, unavailable, Reply.text("ok"))
    (1 to 3).foreach(_ => assert(client.complete(conversation).isLeft))
    assert(client.complete(conversation).isLeft)
    assert(counter.attempts == 3)

  test("the settings: a deadline for every call, and a rate limit only when one is set"):
    val off = Reliability.config("gemini", None)
    assert(off.deadline.contains(Reliability.Deadline))
    assert(!off.rateLimit.enabled)
    val on = Reliability.config("gemini", Some(30))
    assert(on.rateLimit.enabled && on.rateLimit.requestsPerMinute == 30)

  test("TRIAGE_REQUESTS_PER_MINUTE is read and checked"):
    assert(
      AppConfig.load(Map("TRIAGE_REQUESTS_PER_MINUTE" -> "20")).toOption.get.requestsPerMinute.contains(20)
    )
    assert(AppConfig.load(Map("TRIAGE_REQUESTS_PER_MINUTE" -> "0")).isLeft)
    assert(AppConfig.load(Map.empty).toOption.get.requestsPerMinute.isEmpty)
