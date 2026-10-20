# The Triage4s dashboard as a container image (#101).
#
#   docker build -t triage4s .
#   docker run --rm -e OPENAI_API_KEY -p 127.0.0.1:8080:8080 triage4s
#
# Publish the port to the host's loopback only, and on the same number as TRIAGE_PORT: the dashboard accepts only
# requests whose Host is 127.0.0.1:<TRIAGE_PORT> or localhost:<TRIAGE_PORT>. It reads public GitHub issues, never
# writes to GitHub, and keeps the API key on the server.

FROM eclipse-temurin:21-jdk AS build
ARG SBT_VERSION=1.11.7
RUN curl -fsSL "https://github.com/sbt/sbt/releases/download/v${SBT_VERSION}/sbt-${SBT_VERSION}.tgz" \
    | tar -xz -C /opt
ENV PATH="/opt/sbt/bin:${PATH}"
WORKDIR /src
# dependencies first, so they are cached while only the sources change
COPY build.sbt ./
COPY project/build.properties project/plugins.sbt project/
RUN sbt -batch update
COPY modules modules
RUN sbt -batch demo/stage

FROM eclipse-temurin:21-jre
RUN useradd --system --uid 10001 --create-home --home-dir /app triage4s
WORKDIR /app
COPY --from=build /src/modules/demo/target/universal/stage /opt/triage4s
# recordings and saved snapshots live here; the app writes latest.json files under fixtures/
COPY --chown=triage4s:triage4s fixtures /app/fixtures
ENV TRIAGE_BIND_ADDRESS=0.0.0.0 \
    TRIAGE_PORT=8080
USER triage4s
EXPOSE 8080
ENTRYPOINT ["/opt/triage4s/bin/triage4s"]
