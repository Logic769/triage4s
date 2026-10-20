package triage4s.javaapi;

import java.util.ArrayList;
import java.util.List;
import org.llm4s.llmconnect.LLMClient;

/** Plain Java use of the facade, for {@code JTriageSpec} (#97): every call compiles from Java and reads Java types. */
final class JavaTriageCheck {

    private JavaTriageCheck() {}

    /** Triages one issue with the default taxonomy and describes what came back. */
    static List<String> triageOne(LLMClient client) {
        List<String> out = new ArrayList<>();
        try (JTriage triage = JTriage.of(client)) {
            JResult<JTriageResponse> result = triage.triage("42", "CLI crashes", "The command-line launcher crashes on startup.");
            if (!result.isSuccess()) {
                out.add("error:" + result.errorKind().orElseThrow() + ":" + result.error().orElseThrow());
                return out;
            }
            JTriageResponse response = result.get();
            out.add("type:" + response.issueType().orElse("none"));
            out.add("component:" + response.component().orElse("none"));
            out.add("review:" + response.needsReview());
            for (JEvidence evidence : response.evidence()) {
                out.add("evidence:" + evidence.target() + "/" + evidence.field() + "/" + evidence.quote());
            }
            out.add("model:" + response.model());
        }
        return out;
    }

    /** A custom taxonomy built from Java, used for one call; its result's error kind, or "ok". */
    static String withTaxonomy(LLMClient client, JTaxonomy taxonomy) {
        JResult<JTriageResponse> result = JTriage.of(client, taxonomy).triage("1", "CLI crashes", "The launcher crashes.");
        return result.isSuccess() ? "ok" : result.errorKind().orElseThrow().name();
    }

    /** A taxonomy with one type and one component. */
    static JTaxonomy smallTaxonomy() {
        return JTaxonomy.builder("mine-v1")
            .issueType("bug", "Something is broken.")
            .component("cli", "The command line.")
            .build();
    }
}
