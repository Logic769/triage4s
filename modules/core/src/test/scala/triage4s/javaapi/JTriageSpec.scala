package triage4s.javaapi

import org.llm4s.error.AuthenticationError
import org.llm4s.testkit.{Reply, ScriptedLLMClient}
import org.scalatest.funsuite.AnyFunSuite
import scala.jdk.CollectionConverters.*

/** The Java-friendly API (#97): used from plain Java, Java types only, failures as results. */
class JTriageSpec extends AnyFunSuite:
  private val answer =
    """{"issueType":"bug","component":"cli","rationale":"An existing CLI operation fails.",
      |"evidence":[{"target":"issueType","field":"title","quote":"crashes"},
      |{"target":"component","field":"title","quote":"CLI"}],"needsReview":false,"reviewReasons":[]}""".stripMargin

  private def answering(text: String) = ScriptedLLMClient.always(Reply.text(text).withModel("test-model"))

  test("Java code triages an issue and reads the suggestion with Java types"):
    assert(
      JavaTriageCheck.triageOne(answering(answer)).asScala.toList == List(
        "type:bug",
        "component:cli",
        "review:false",
        "evidence:issueType/title/crashes",
        "evidence:component/title/CLI",
        "model:test-model"
      )
    )

  test("a provider failure is a PROVIDER_FAILURE result, not an exception"):
    val client = ScriptedLLMClient.always(Reply.failure(AuthenticationError("openai", "bad key")))
    val out = JavaTriageCheck.triageOne(client).asScala.toList
    assert(out.size == 1 && out.head.startsWith("error:PROVIDER_FAILURE:"))

  test("malformed model output is an INVALID_OUTPUT result"):
    assert(JavaTriageCheck.triageOne(answering("{}")).asScala.head.startsWith("error:INVALID_OUTPUT:"))

  test("blank or null input is an INVALID_INPUT result, and the model is not called"):
    val client = answering(answer)
    val triage = JTriage.of(client)
    assert(triage.triage("1", " ", "").errorKind().get() == JErrorKind.INVALID_INPUT)
    assert(triage.triage(null, "t", "b").errorKind().get() == JErrorKind.INVALID_INPUT)
    assert(client.callCount == 0)

  test("a custom taxonomy from Java is used, and an invalid one is an INVALID_TAXONOMY result"):
    val client = answering(answer)
    assert(JavaTriageCheck.withTaxonomy(client, JavaTriageCheck.smallTaxonomy()) == "ok")
    assert(client.lastCall.get.messages.head.content.contains("The command line."))
    val empty = JTaxonomy.builder("empty").build()
    assert(JavaTriageCheck.withTaxonomy(client, empty) == "INVALID_TAXONOMY")
    assert(JTaxonomy.defaultTaxonomy().version() == "copilot-lens-v1")

  test("JResult reads a success and a failure, and get() on a failure says why"):
    val ok = JTriage.of(answering(answer)).triage("1", "CLI crashes", "x")
    assert(ok.isSuccess() && ok.getOrNull() != null && ok.error().isEmpty && ok.errorKind().isEmpty)
    val failed = JTriage.of(answering("{}")).triage("1", "CLI crashes", "x")
    assert(!failed.isSuccess() && failed.getOrNull() == null)
    val thrown = intercept[IllegalStateException](failed.get())
    assert(thrown.getMessage.startsWith("INVALID_OUTPUT:"))
    assert(failed.toString.startsWith("JResult.failure(INVALID_OUTPUT"))

  test("fromConfiguration reports an unknown or blank section as a CONFIGURATION result"):
    assert(JTriage.fromConfiguration("no-such-section").errorKind().get() == JErrorKind.CONFIGURATION)
    assert(JTriage.fromConfiguration(" ").errorKind().get() == JErrorKind.CONFIGURATION)

  test("no public signature of the Java API mentions a scala.* type"):
    val classes = Seq(
      classOf[JTriage],
      classOf[JResult[?]],
      classOf[JTriageResponse],
      classOf[JEvidence],
      classOf[JTaxonomy],
      classOf[JTaxonomy.Builder],
      classOf[JErrorKind]
    )
    val leaks = for
      cls <- classes
      m <- cls.getMethods.toSeq if m.getDeclaringClass != classOf[Object] && !m.getDeclaringClass.isEnum
      t <- m.getGenericParameterTypes.toSeq :+ m.getGenericReturnType
      if t.getTypeName.contains("scala.")
    yield s"${cls.getSimpleName}.${m.getName}: ${t.getTypeName}"
    assert(leaks.isEmpty, leaks.mkString("\n"))
