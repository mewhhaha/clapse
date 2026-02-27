export function buildCompileRequest(inputSource) {
  return {
    command: "compile",
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
