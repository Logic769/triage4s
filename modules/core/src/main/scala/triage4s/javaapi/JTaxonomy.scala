package triage4s.javaapi

import triage4s.{Category, Taxonomy}

/** A classification taxonomy built from Java (#97): the issue types and components the model may suggest. */
final class JTaxonomy private[javaapi] (private[javaapi] val taxonomy: Taxonomy):
  def version(): String = taxonomy.version
  override def toString: String =
    s"JTaxonomy(${taxonomy.version}, types=${taxonomy.types.map(_.id).mkString(",")}, " +
      s"components=${taxonomy.components.map(_.id).mkString(",")})"

object JTaxonomy:
  /** The built-in taxonomy of the demo (copilot-lens). */
  def defaultTaxonomy(): JTaxonomy = new JTaxonomy(Taxonomy.copilotLens)

  /** A builder for a taxonomy called `version`. It is checked when a triage call uses it, not here. */
  def builder(version: String): Builder = new Builder(Taxonomy(version, Vector.empty, Vector.empty))

  /** A taxonomy under construction. Immutable: every method returns a new builder. */
  final class Builder private[javaapi] (taxonomy: Taxonomy):
    /** An issue type the model may suggest, with what it means. */
    def issueType(id: String, description: String): Builder =
      new Builder(taxonomy.copy(types = taxonomy.types :+ Category(id, description)))

    /** A component the model may suggest, with what it covers. */
    def component(id: String, description: String): Builder =
      new Builder(taxonomy.copy(components = taxonomy.components :+ Category(id, description)))
    def build(): JTaxonomy = new JTaxonomy(taxonomy)
