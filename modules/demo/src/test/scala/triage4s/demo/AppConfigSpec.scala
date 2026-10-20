package triage4s.demo

import org.scalatest.funsuite.AnyFunSuite

class AppConfigSpec extends AnyFunSuite:
  test("defaults work without credentials and never print configured keys"):
    val empty = AppConfig.load(Map.empty).toOption.get
    assert(empty.port == 8080)
    assert(empty.model == "gpt-4o-mini")
    assert(AppConfig.client(empty).isLeft)
    val configured = AppConfig.load(Map("OPENAI_API_KEY" -> "test-secret")).toOption.get
    assert(!configured.toString.contains("test-secret"))

  test("invalid ports and model names are explicit errors"):
    Vector("0", "80", "65536", "oops").foreach { port =>
      assert(AppConfig.load(Map("TRIAGE_PORT" -> port)).isLeft)
    }
    assert(AppConfig.load(Map("TRIAGE_MODEL" -> "../bad model")).isLeft)

  test("port boundaries are inclusive from 1024 to 65535"):
    Vector(1024, 65535).foreach { port =>
      val config = AppConfig.load(Map("TRIAGE_PORT" -> port.toString)).toOption.get
      assert(config.port == port)
    }
    Vector(1023, 65536).foreach { port =>
      assert(AppConfig.load(Map("TRIAGE_PORT" -> port.toString)).isLeft)
    }

  test("surrounding whitespace is trimmed from port and model"):
    val config = AppConfig
      .load(
        Map(
          "TRIAGE_PORT" -> " \t9000\n",
          "TRIAGE_MODEL" -> " \ttest-model\n"
        )
      )
      .toOption
      .get
    assert(config.port == 9000)
    assert(config.model == "test-model")

  test("empty and whitespace-only port and model values use defaults"):
    Vector("", " \t\n").foreach { blank =>
      val config = AppConfig.load(Map("TRIAGE_PORT" -> blank, "TRIAGE_MODEL" -> blank)).toOption.get
      assert(config.port == 8080)
      assert(config.model == "gpt-4o-mini")
    }

  test("missing, empty, and whitespace-only credentials are absent"):
    val missing = AppConfig.load(Map.empty).toOption.get
    assert(missing.apiKey.isEmpty)
    assert(missing.githubToken.isEmpty)
    Vector("", " \t\n").foreach { blank =>
      val config = AppConfig.load(Map("OPENAI_API_KEY" -> blank, "GITHUB_TOKEN" -> blank)).toOption.get
      assert(config.apiKey.isEmpty)
      assert(config.githubToken.isEmpty)
    }

  test("both credentials are trimmed and redacted from configuration text"):
    val fakeApiKey = "fake-openai-key-for-tests"
    val fakeGithubToken = "fake-github-token-for-tests"
    val config = AppConfig
      .load(
        Map(
          "OPENAI_API_KEY" -> s" \t$fakeApiKey\n",
          "GITHUB_TOKEN" -> s" \t$fakeGithubToken\n"
        )
      )
      .toOption
      .get
    assert(config.apiKey.contains(fakeApiKey))
    assert(config.githubToken.contains(fakeGithubToken))
    assert(!config.toString.contains(fakeApiKey))
    assert(!config.toString.contains(fakeGithubToken))

  test("the OpenAI configuration targets HTTPS and bounds each attempt with the request timeout"):
    import org.llm4s.llmconnect.config.ContextWindowResolver
    import org.llm4s.model.ModelRegistryService
    given ModelRegistryService = ModelRegistryService.default().toOption.get
    given ContextWindowResolver = ContextWindowResolver(summon[ModelRegistryService])
    val config = AppConfig.load(Map("TRIAGE_MODEL" -> "gpt-4o-mini")).toOption.get
    val provider = AppConfig.providerConfig(config, "fake-openai-key-for-tests").toOption.get
    assert(provider.baseUrl == "https://api.openai.com/v1")
    assert(provider.model == "gpt-4o-mini")
    assert(provider.timeouts.request.contains(AppConfig.RequestTimeout))
    assert(AppConfig.TransportNotice.contains("up to 2 times"))

  test("the dashboard listens on loopback unless TRIAGE_BIND_ADDRESS says 0.0.0.0, for a container (#101)"):
    assert(AppConfig.load(Map.empty).toOption.get.bindAddress == "127.0.0.1")
    assert(AppConfig.load(Map("TRIAGE_BIND_ADDRESS" -> " 0.0.0.0 ")).toOption.get.bindAddress == "0.0.0.0")
    Seq("10.0.0.5", "example.com", "::").foreach { address =>
      assert(
        AppConfig
          .load(Map("TRIAGE_BIND_ADDRESS" -> address))
          .left
          .toOption
          .contains("TRIAGE_BIND_ADDRESS must be 127.0.0.1, or 0.0.0.0 inside a container.")
      )
    }
