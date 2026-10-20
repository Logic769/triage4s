package triage4s.cli

import java.nio.file.Files
import org.llm4s.error.AuthenticationError
import org.llm4s.testkit.{Reply, ScriptedLLMClient}
import org.scalatest.funsuite.AnyFunSuite
import scala.collection.mutable.ArrayBuffer

/** `triage4s classify` (#99): arguments, input, output and exit codes, with a scripted model and no network.
  */
class CliSpec extends AnyFunSuite:
  private val suggested =
    """{"issueType":"bug","component":"cli","rationale":"An existing CLI operation fails.",
      |"evidence":[{"target":"issueType","field":"title","quote":"crashes"},
      |{"target":"component","field":"title","quote":"CLI"}],"needsReview":false,"reviewReasons":[]}""".stripMargin
  private val review =
    """{"issueType":null,"component":null,"rationale":"Too vague.","evidence":[],"needsReview":true,
      |"reviewReasons":["The issue does not say what fails."]}""".stripMargin

  private def issueFile(): String =
    val file = Files.createTempFile("issue", ".json")
    Files.writeString(file, """{"id":"42","title":"CLI crashes","body":"The launcher crashes on startup."}""")
    file.toString

  private final case class Ran(code: Int, out: String, err: String, providers: Seq[(String, Option[String])])

  private def run(
      args: Seq[String],
      reply: Reply = Reply.text(suggested).withModel("test-model"),
      fetch: (String, Map[String, String]) => Either[String, String] = (_, _) => Left("no network in tests")
  ): Ran =
    val (out, err, asked) =
      (ArrayBuffer.empty[String], ArrayBuffer.empty[String], ArrayBuffer.empty[(String, Option[String])])
    val code = Cli.run(
      args,
      out += _,
      err += _,
      clientFor = (section, model) => { asked += section -> model; Right(ScriptedLLMClient.always(reply)) },
      fetch = fetch
    )
    Ran(code, out.mkString("\n"), err.mkString("\n"), asked.toSeq)

  test("classifies an issue from a file and prints the suggestion with its evidence"):
    val ran = run(Seq("classify", "--file", issueFile()))
    assert(ran.code == 0)
    assert(ran.out.contains("type:      bug"))
    assert(ran.out.contains("component: cli"))
    assert(ran.out.contains("""evidence:  issueType from title: "crashes""""))
    assert(ran.providers == Seq("openai" -> None))

  test("--json prints the documented result contract"):
    val ran = run(Seq("classify", "--file", issueFile(), "--json"))
    val json = ujson.read(ran.out)
    assert(json("id").str == "42")
    assert(json("response")("result")("issueType").str == "bug")
    assert(json("response")("model").str == "test-model")

  test("exit code 2 when the result needs human review"):
    val ran = run(Seq("classify", "--file", issueFile()), Reply.text(review))
    assert(ran.code == 2)
    assert(ran.out.contains("NEEDS HUMAN REVIEW"))
    assert(ran.out.contains("The issue does not say what fails."))

  test("exit code 1, with the reason, for a provider error"):
    val ran = run(Seq("classify", "--file", issueFile()), Reply.failure(AuthenticationError("openai", "bad")))
    assert(ran.code == 1)
    assert(ran.err.contains("LLM request failed"))

  test("--provider and --model choose the LLM4S section and model"):
    val ran = run(Seq("classify", "--file", issueFile(), "--provider", "ollama", "--model", "llama3.2"))
    assert(ran.providers == Seq("ollama" -> Some("llama3.2")))

  test("reads a GitHub issue read-only, and refuses a pull request"):
    val seen = ArrayBuffer.empty[String]
    val issue = """{"title":"CLI crashes","body":"The launcher crashes on startup."}"""
    val ran = run(
      Seq("classify", "--repo", "owner/name", "--issue", "7"),
      fetch = (url, headers) => { seen += url; assert(headers("User-Agent") == "triage4s-cli"); Right(issue) }
    )
    assert(ran.code == 0)
    assert(seen.toSeq == Seq("https://api.github.com/repos/owner/name/issues/7"))
    assert(ran.out.startsWith("owner/name#7: CLI crashes"))
    val pr = run(
      Seq("classify", "--repo", "o/n", "--issue", "8"),
      fetch = (_, _) => Right("""{"pull_request":{}}""")
    )
    assert(pr.code == 1 && pr.err.contains("is a pull request"))
    val down =
      run(Seq("classify", "--repo", "o/n", "--issue", "9"), fetch = (_, _) => Left("GitHub answered 404."))
    assert(down.code == 1 && down.err.contains("404"))

  test("a custom taxonomy file is used, and an unreadable one is an error"):
    val file = Files.createTempFile("taxonomy", ".json")
    Files.writeString(
      file,
      """{"version":"mine","types":[{"id":"bug","description":"Broken."}],"components":[{"id":"cli","description":"CLI."}]}"""
    )
    assert(run(Seq("classify", "--file", issueFile(), "--taxonomy", file.toString)).code == 0)
    val bad = run(Seq("classify", "--file", issueFile(), "--taxonomy", "/no/such/taxonomy.json"))
    assert(bad.code == 1 && bad.err.contains("Could not read the taxonomy"))

  test("bad arguments print the problem and the usage, with exit code 1"):
    Seq(
      Seq.empty[String] -> "No command given.",
      Seq("triage") -> "Unknown command 'triage'.",
      Seq("classify") -> "Give an issue with --file",
      Seq("classify", "--file") -> "--file needs a value.",
      Seq("classify", "--file", "a", "--file", "b") -> "--file is given twice.",
      Seq("classify", "--file", "a", "--repo", "o/n", "--issue", "1") -> "not both",
      Seq("classify", "--repo", "o/n") -> "--repo needs --issue.",
      Seq("classify", "--issue", "1") -> "--issue needs --repo.",
      Seq("classify", "--repo", "bad repo", "--issue", "1") -> "--repo must be OWNER/NAME",
      Seq("classify", "--repo", "o/n", "--issue", "x") -> "--issue must be a positive number",
      Seq("classify", "--bogus") -> "Unknown option '--bogus'."
    ).foreach { (args, problem) =>
      val ran = run(args)
      assert(ran.code == 1, args)
      assert(ran.err.contains(problem), s"$args: ${ran.err}")
      assert(ran.err.contains("Usage: triage4s classify"))
    }

  test("an unreadable issue file is an error naming the expected shape"):
    val file = Files.createTempFile("issue", ".json")
    Files.writeString(file, "not json")
    val ran = run(Seq("classify", "--file", file.toString))
    assert(ran.code == 1 && ran.err.contains("""{"id", "title", "body"}"""))

  test("the default client reports an unknown provider section"):
    assert(Cli.client("no-such-section", None).left.toOption.exists(_.contains("no-such-section")))
