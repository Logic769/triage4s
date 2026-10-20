package triage4s.demo

import org.scalatest.funsuite.AnyFunSuite

/** Choosing the model provider with TRIAGE_PROVIDER and the LLM4S sections in application.conf (#88). */
class ProviderSelectionSpec extends AnyFunSuite:
  private def configured(env: (String, String)*) =
    AppConfig.load(env.toMap).flatMap(AppConfig.configured)

  test("a named section gives a client for its provider and model, with no API key and no network"):
    val selected = configured("TRIAGE_PROVIDER" -> "ollama").toOption.get
    try
      assert(selected.provider == "ollama")
      assert(selected.model == "qwen2.5:7b")
    finally selected.client.close()

  test("TRIAGE_MODEL replaces the section's model"):
    val selected = configured("TRIAGE_PROVIDER" -> "ollama", "TRIAGE_MODEL" -> "llama3.2").toOption.get
    try assert(selected.model == "llama3.2")
    finally selected.client.close()

  test("an unknown section is a configuration error naming it, not a crash"):
    val error = configured("TRIAGE_PROVIDER" -> "no-such-section").left.toOption.get
    assert(error.contains("no-such-section"))

  test("an invalid section name is refused when the configuration loads"):
    assert(
      AppConfig
        .load(Map("TRIAGE_PROVIDER" -> "../etc"))
        .left
        .toOption
        .contains("TRIAGE_PROVIDER has an invalid format.")
    )

  test("without TRIAGE_PROVIDER, OpenAI with OPENAI_API_KEY is used as before"):
    val selected = configured("OPENAI_API_KEY" -> "fake-openai-key-for-tests").toOption.get
    try
      assert(selected.provider == "openai")
      assert(selected.model == "gpt-4o-mini")
    finally selected.client.close()
    assert(configured().left.toOption.exists(_.contains("TRIAGE_PROVIDER")))

  test("the configuration's description redacts credentials and names the provider"):
    val config =
      AppConfig.load(Map("TRIAGE_PROVIDER" -> "anthropic", "OPENAI_API_KEY" -> "secret-value")).toOption.get
    assert(config.toString.contains("provider=anthropic"))
    assert(!config.toString.contains("secret-value"))
