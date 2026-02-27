export function buildCompileRequest(inputSource, compileMode = null) {
  const request = {
    command: "compile",
    input_path: "repl/input.clapse",
    input_source: inputSource,
    plugin_wasm_paths: [],
  };
  if (typeof compileMode === "string" && compileMode.length > 0) {
    request.compile_mode = compileMode;
  }
  return request;
}

export function buildArtifactsRequest(inputSource) {
  return {
    command: "selfhost-artifacts",
    input_path: "repl/input.clapse",
    input_source: inputSource,
  };
}

export function isCompileResponse(response) {
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
  return true;
}

export function tryDebugCompile(session, inputSource) {
  try {
    const response = session.call(buildCompileRequest(inputSource, "debug"));
    if (isCompileResponse(response)) {
      return { ok: true, response };
    }
    const error =
      response &&
      typeof response === "object" &&
      typeof response.error === "string"
        ? response.error
        : "unexpected compile response";
    return { ok: false, error };
  } catch (err) {
    const error = err instanceof Error ? err.message : String(err);
    return { ok: false, error };
  }
}

export function runArtifactsPipeline(session, inputSource) {
  let artifactsResponse = null;
  let artifactsError = null;
  try {
    artifactsResponse = session.call(buildArtifactsRequest(inputSource));
  } catch (err) {
    artifactsError = err instanceof Error ? err.message : String(err);
  }
  return { artifactsResponse, artifactsError };
}

export function runCompilePipeline(session, inputSource) {
  const compileResponse = session.call(buildCompileRequest(inputSource));
  const { artifactsResponse, artifactsError } = runArtifactsPipeline(
    session,
    inputSource,
  );
  return { compileResponse, artifactsResponse, artifactsError };
}
