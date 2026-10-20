package triage4s.javaapi;

/** Why a {@link JResult} failed (#97): one constant per kind of triage error, so Java code can {@code switch} on it. */
public enum JErrorKind {
    /** The issue itself is unusable: blank, or longer than the input limit. */
    INVALID_INPUT,
    /** The taxonomy is invalid: empty, duplicated or malformed categories. */
    INVALID_TAXONOMY,
    /** The model answered, but its output failed validation (malformed JSON, unknown category, missing evidence). */
    INVALID_OUTPUT,
    /** The provider call failed: credentials, quota, model or connectivity. */
    PROVIDER_FAILURE,
    /** The LLM4S provider configuration could not be loaded or used. */
    CONFIGURATION
}
