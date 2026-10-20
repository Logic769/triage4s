package triage4s.cli

import java.net.URI
import java.net.http.{HttpClient, HttpRequest, HttpResponse}
import java.nio.file.{Files, Path}
import java.time.Duration
import org.llm4s.config.Llm4sConfig
import org.llm4s.llmconnect.{LLMClient, LLMConnect}
import scala.util.{Try, Using}
import triage4s.*

/** Where the issue comes from. */
enum Source:
  case File(path: Path)
  case GitHub(repository: String, number: Int)

/** A parsed `classify` command. */
final case class Command(
    source: Source,
    provider: String,
    model: Option[String],
    taxonomy: Option[Path],
    json: Boolean
)

/** `triage4s classify`: suggests a type and component for one issue, with the evidence for each, from the
  * terminal or a script (#99). Read-only: it never writes to GitHub.
  *
  * Exit codes: 0 a suggestion was made, 2 it needs human review, 1 an error (bad arguments, input, provider,
  * output).
  */
object Cli:
  val Usage: String =
    """Usage: triage4s classify (--file ISSUE.json | --repo OWNER/NAME --issue NUMBER) [options]
      |
      |  --file PATH        an issue as JSON: {"id": "...", "title": "...", "body": "..."}
      |  --repo OWNER/NAME  a public GitHub repository, with --issue NUMBER (read-only; GITHUB_TOKEN is used if set)
      |  --provider NAME    an LLM4S provider section (default: openai); see application.conf
      |  --model MODEL      replaces the section's model
      |  --taxonomy PATH    a taxonomy as JSON (default: the built-in copilot-lens taxonomy)
      |  --json             print the result as JSON
      |
      |Exit codes: 0 suggested, 2 needs human review, 1 error.""".stripMargin

  /** The command `args` describe, or what is wrong with them. */
  def parse(args: Seq[String]): Either[String, Command] =
    args.toList match
      case "classify" :: rest => options(rest, Map.empty, json = false)
      case Nil                => Left("No command given.")
      case other :: _         => Left(s"Unknown command '$other'.")

  private val Valued = Set("--file", "--repo", "--issue", "--provider", "--model", "--taxonomy")

  @scala.annotation.tailrec
  private def options(rest: List[String], seen: Map[String, String], json: Boolean): Either[String, Command] =
    rest match
      case "--json" :: tail => options(tail, seen, json = true)
      case flag :: value :: tail if Valued(flag) && !value.startsWith("--") =>
        if seen.contains(flag) then Left(s"$flag is given twice.")
        else options(tail, seen + (flag -> value), json)
      case flag :: _ if Valued(flag) => Left(s"$flag needs a value.")
      case other :: _                => Left(s"Unknown option '$other'.")
      case Nil                       => command(seen, json)

  private def command(seen: Map[String, String], json: Boolean): Either[String, Command] =
    val source = (seen.get("--file"), seen.get("--repo"), seen.get("--issue")) match
      case (Some(file), None, None)        => Right(Source.File(Path.of(file)))
      case (None, Some(repo), Some(issue)) =>
        for
          _ <- Either.cond(
            repo.matches("[A-Za-z0-9_.-]+/[A-Za-z0-9_.-]+"),
            (),
            s"--repo must be OWNER/NAME, got '$repo'."
          )
          n <- issue.toIntOption.filter(_ > 0).toRight(s"--issue must be a positive number, got '$issue'.")
        yield Source.GitHub(repo, n)
      case (None, Some(_), None) => Left("--repo needs --issue.")
      case (None, None, Some(_)) => Left("--issue needs --repo.")
      case (None, None, None)    => Left("Give an issue with --file, or with --repo and --issue.")
      case _                     => Left("Give either --file, or --repo and --issue, not both.")
    source.map(
      Command(
        _,
        seen.getOrElse("--provider", "openai"),
        seen.get("--model"),
        seen.get("--taxonomy").map(Path.of(_)),
        json
      )
    )

  /** The issue `source` names. `fetch` reads a URL, with headers; injectable for tests. */
  def load(
      source: Source,
      fetch: (String, Map[String, String]) => Either[String, String]
  ): Either[String, TriageInput] =
    source match
      case Source.File(path) =>
        Try(upickle.default.read[TriageInput](Files.readString(path))).toEither.left
          .map(e => s"Could not read $path as {\"id\", \"title\", \"body\"}: ${e.getMessage}")
      case Source.GitHub(repo, number) =>
        val token = sys.env.get("GITHUB_TOKEN").map(_.trim).filter(_.nonEmpty)
        val headers = Map("Accept" -> "application/vnd.github+json", "User-Agent" -> "triage4s-cli") ++
          token.map(t => "Authorization" -> s"Bearer $t")
        for
          text <- fetch(s"https://api.github.com/repos/$repo/issues/$number", headers)
          json <- Try(ujson.read(text)).toEither.left.map(_ =>
            s"GitHub returned something that is not JSON for $repo#$number."
          )
          _ <- Either.cond(
            !json.obj.contains("pull_request"),
            (),
            s"$repo#$number is a pull request, not an issue."
          )
        yield TriageInput(
          s"$repo#$number",
          json.obj.get("title").flatMap(_.strOpt).getOrElse(""),
          json.obj.get("body").flatMap(_.strOpt).getOrElse("")
        )

  /** The taxonomy in `path`, or the built-in one. */
  def taxonomy(path: Option[Path]): Either[String, Taxonomy] =
    path.fold(Right(Taxonomy.copilotLens))(p =>
      Try(upickle.default.read[Taxonomy](Files.readString(p))).toEither.left.map(e =>
        s"Could not read the taxonomy $p: ${e.getMessage}"
      )
    )

  /** The client of LLM4S provider section `section`, its model replaced by `model` when set. */
  def client(section: String, model: Option[String]): Either[String, LLMClient] =
    for
      registry <- Llm4sConfig.modelRegistryService().left.map(_.message)
      provider <- Llm4sConfig.provider(section).left.map(e => s"Provider section '$section': ${e.message}")
      client <- LLMConnect
        .getClient(model.fold(provider)(provider.withModel))(using registry)
        .left
        .map(_.message)
    yield client

  /** The text a terminal shows for `response`. */
  def text(input: TriageInput, response: TriageResponse): String =
    val r = response.result
    val lines = Vector(
      s"${input.id}: ${input.title}",
      s"  type:      ${r.issueType.getOrElse("(abstained)")}",
      s"  component: ${r.component.getOrElse("(abstained)")}",
      s"  rationale: ${r.rationale}"
    ) ++ r.evidence.map(e => s"""  evidence:  ${e.target} from ${e.field}: "${e.quote}"""") ++
      (if r.needsReview then s"  NEEDS HUMAN REVIEW" +: r.reviewReasons.map(reason => s"    - $reason")
       else Vector.empty) :+
      s"  model:     ${response.model}"
    lines.mkString("\n")

  /** The JSON the `--json` flag prints: the issue's id and the triage response, as replay files store it. */
  def json(input: TriageInput, response: TriageResponse): String =
    ujson
      .Obj("id" -> input.id, "title" -> input.title, "response" -> upickle.default.writeJs(response))
      .render(indent = 2)

  /** Runs `args`, printing to `out` and `err`; returns the exit code. */
  def run(
      args: Seq[String],
      out: String => Unit,
      err: String => Unit,
      clientFor: (String, Option[String]) => Either[String, LLMClient] = client,
      fetch: (String, Map[String, String]) => Either[String, String] = httpGet
  ): Int =
    parse(args) match
      case Left(problem) =>
        err(s"$problem\n\n$Usage")
        1
      case Right(command) =>
        val outcome = for
          input <- load(command.source, fetch)
          tax <- taxonomy(command.taxonomy)
          client <- clientFor(command.provider, command.model)
          response <- Using(client)(c => new Triage(c).triage(input, tax)).toEither.left
            .map(e => s"The provider call failed (${e.getClass.getSimpleName}).")
            .flatMap(_.left.map(_.message))
        yield input -> response
        outcome match
          case Left(problem) =>
            err(problem)
            1
          case Right((input, response)) =>
            out(if command.json then json(input, response) else text(input, response))
            if response.result.needsReview then 2 else 0

  private def httpGet(url: String, headers: Map[String, String]): Either[String, String] =
    val client = HttpClient
      .newBuilder()
      .connectTimeout(Duration.ofSeconds(10))
      .followRedirects(HttpClient.Redirect.NEVER)
      .build()
    val request = headers.foldLeft(
      HttpRequest.newBuilder(URI.create(url)).timeout(Duration.ofSeconds(30)).GET()
    )((b, h) => b.header(h._1, h._2))
    Try(client.send(request.build(), HttpResponse.BodyHandlers.ofString())).toEither.left
      .map(e => s"Could not reach GitHub (${e.getClass.getSimpleName}).")
      .flatMap(r => Either.cond(r.statusCode == 200, r.body, s"GitHub answered ${r.statusCode} for $url."))

object Main:
  def main(args: Array[String]): Unit =
    sys.exit(Cli.run(args.toIndexedSeq, println, System.err.println))
