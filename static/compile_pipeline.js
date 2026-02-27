export function buildCompileRequest(inputSource) {
  return {
    command: "compile",
    input_path: "repl/input.clapse",
    input_source: inputSource,
    plugin_wasm_paths: [],
  };
}

export function buildUnifiedCompileRequest(command, inputSource) {
  return {
    command,
    input_path: "repl/input.clapse",
    input_source: inputSource,
    plugin_wasm_paths: [],
  };
}

export function buildArtifactsRequest(inputSource) {
  return {
    command: "selfhost-artifacts",
    input_path: "repl/input.clapse",
    input_source: inputSource,
  };
}

export function isUnifiedCompileResponse(response) {
  if (!response || typeof response !== "object" || Array.isArray(response)) {
    return false;
  }
  if (response.ok !== true) {
    return false;
  }
  if (
    typeof response.wasm_base64 !== "string" ||
    response.wasm_base64.length < 1
  ) {
    return false;
  }
  if (!response.artifacts || typeof response.artifacts !== "object") {
    return false;
  }
  return true;
}

export function tryUnifiedCompileDebug(session, inputSource) {
  const command = "compile-debug";
  try {
    const response = session.call(
      buildUnifiedCompileRequest(command, inputSource),
    );
    if (isUnifiedCompileResponse(response)) {
      return { ok: true, command, response };
    }
    const error =
      response &&
      typeof response === "object" &&
      typeof response.error === "string"
        ? response.error
        : "unexpected unified response";
    return { ok: false, errors: [{ command, error }] };
  } catch (err) {
    const error = err instanceof Error ? err.message : String(err);
    return { ok: false, errors: [{ command, error }] };
  }
}

export function runCompilePipeline(session, inputSource) {
  const compileResponse = session.call(buildCompileRequest(inputSource));

  let artifactsResponse = null;
  let artifactsError = null;
  try {
    artifactsResponse = session.call(buildArtifactsRequest(inputSource));
  } catch (err) {
    artifactsError = err instanceof Error ? err.message : String(err);
  }

  return { compileResponse, artifactsResponse, artifactsError };
}
