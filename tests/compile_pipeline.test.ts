import {
  runCompilePipeline,
  tryUnifiedCompileDebug,
} from "../static/compile_pipeline.js";

Deno.test(
  "runCompilePipeline keeps compile result when artifacts throw",
  () => {
    const calls: string[] = [];
    const session = {
      call(request: { command: string }) {
        calls.push(request.command);
        if (request.command === "compile") {
          return { ok: true, wasm_base64: "AA==" };
        }
        if (request.command === "selfhost-artifacts") {
          throw new RangeError("too much recursion");
        }
        throw new Error(`Unexpected command: ${request.command}`);
      },
    };

    const result = runCompilePipeline(session, "main = 1");
    if (result.compileResponse?.ok !== true) {
      throw new Error("Expected compile response to be successful.");
    }
    if (result.artifactsResponse !== null) {
      throw new Error("Expected no artifacts response when command throws.");
    }
    if (result.artifactsError !== "too much recursion") {
      throw new Error(`Unexpected artifacts error: ${result.artifactsError}`);
    }
    if (calls.join(",") !== "compile,selfhost-artifacts") {
      throw new Error(`Unexpected command order: ${calls.join(",")}`);
    }
  },
);

Deno.test(
  "runCompilePipeline returns both responses when commands succeed",
  () => {
    const calls: string[] = [];
    const session = {
      call(request: { command: string }) {
        calls.push(request.command);
        if (request.command === "compile") {
          return { ok: true };
        }
        if (request.command === "selfhost-artifacts") {
          return { ok: true, artifacts: {} };
        }
        throw new Error(`Unexpected command: ${request.command}`);
      },
    };

    const result = runCompilePipeline(session, "main = 1");
    if (result.compileResponse?.ok !== true) {
      throw new Error("Expected successful compile response.");
    }
    if (result.artifactsResponse?.ok !== true) {
      throw new Error("Expected successful artifacts response.");
    }
    if (result.artifactsError !== null) {
      throw new Error(
        `Expected null artifactsError, got: ${result.artifactsError}`,
      );
    }
    if (calls.join(",") !== "compile,selfhost-artifacts") {
      throw new Error(`Unexpected command order: ${calls.join(",")}`);
    }
  },
);

Deno.test(
  "tryUnifiedCompileDebug returns unified response when available",
  () => {
    const calls: string[] = [];
    const session = {
      call(request: { command: string }) {
        calls.push(request.command);
        if (request.command === "compile-debug") {
          return {
            ok: true,
            wasm_base64: "AA==",
            artifacts: {
              "lowered_ir.txt": "lowered",
              "collapsed_ir.txt": "collapsed",
            },
          };
        }
        throw new Error(`Unexpected command: ${request.command}`);
      },
    };

    const result = tryUnifiedCompileDebug(session, "main = 1");
    if (!result.ok) {
      throw new Error("Expected unified compile command to succeed.");
    }
    if (result.command !== "compile-debug") {
      throw new Error(`Unexpected unified command: ${result.command}`);
    }
    if (result.response.ok !== true) {
      throw new Error("Expected unified response ok=true.");
    }
    if (calls.join(",") !== "compile-debug") {
      throw new Error(`Unexpected command call order: ${calls.join(",")}`);
    }
  },
);

Deno.test("tryUnifiedCompileDebug reports unsupported command", () => {
  const session = {
    call(_request: { command: string }) {
      return { ok: false, error: "unsupported command" };
    },
  };

  const result = tryUnifiedCompileDebug(session, "main = 1");
  if (result.ok) {
    throw new Error("Expected unified compile command to fail.");
  }
  const errors = result.errors ?? [];
  if (errors.length !== 1) {
    throw new Error(`Expected one probe error, got ${errors.length}`);
  }
  if (errors[0].error !== "unsupported command") {
    throw new Error(`Unexpected probe error: ${errors[0].error}`);
  }
});
