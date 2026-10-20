package triage4s.javaapi

import java.util.{List as JList, Optional, OptionalInt}
import scala.jdk.CollectionConverters.*
import triage4s.{Evidence, TriageResponse}

/** One exact excerpt that supports a classification: which classification, which field, and the quote. */
final class JEvidence private[javaapi] (evidence: Evidence):
  /** `issueType` or `component`. */
  def target(): String = evidence.target

  /** `title` or `body`. */
  def field(): String = evidence.field

  /** The exact excerpt. */
  def quote(): String = evidence.quote
  override def toString: String = s"JEvidence(${evidence.target}, ${evidence.field}, ${evidence.quote})"

/** A triage suggestion, read from Java (#97): the suggested type and component (empty when the model
  * abstained), the rationale, the evidence, whether a human must review it and why, and the model and token
  * usage.
  */
final class JTriageResponse private[javaapi] (response: TriageResponse):
  def issueType(): Optional[String] = response.result.issueType.fold(Optional.empty[String]())(Optional.of)
  def component(): Optional[String] = response.result.component.fold(Optional.empty[String]())(Optional.of)
  def rationale(): String = response.result.rationale
  def evidence(): JList[JEvidence] = JList.copyOf(response.result.evidence.map(new JEvidence(_)).asJava)
  def needsReview(): Boolean = response.result.needsReview
  def reviewReasons(): JList[String] = JList.copyOf(response.result.reviewReasons.asJava)
  def model(): String = response.model

  /** Total tokens the call used, when the provider reported them. */
  def totalTokens(): OptionalInt = response.usage.fold(OptionalInt.empty())(u => OptionalInt.of(u.total))
  override def toString: String =
    s"JTriageResponse(type=${response.result.issueType.orNull}, component=${response.result.component.orNull}, " +
      s"needsReview=${response.result.needsReview})"
