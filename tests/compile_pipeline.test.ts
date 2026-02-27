import {
  runCompilePipeline,
  tryDebugCompile,
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
  "tryDebugCompile sends compile request with compile_mode=debug",
  () => {
    const calls: string[] = [];
    const session = {
      call(request: { command: string; compile_mode?: string }) {
        calls.push(`${request.command}:${request.compile_mode ?? ""}`);
        if (request.command === "compile" && request.compile_mode === "debug") {
          return {
            ok: true,
            wasm_base64: "AA==",
          };
        }
        throw new Error(`Unexpected command: ${request.command}`);
      },
    };

    const result = tryDebugCompile(session, "main = 1");
    if (!result.ok) {
      throw new Error("Expected debug compile request to succeed.");
    }
    if (result.response.ok !== true) {
      throw new Error("Expected compile response ok=true.");
    }
    if (calls.join(",") !== "compile:debug") {
      throw new Error(`Unexpected command call order: ${calls.join(",")}`);
    }
  },
);

Deno.test("tryDebugCompile reports compiler error", () => {
  const session = {
    call(_request: { command: string }) {
      return { ok: false, error: "unsupported command" };
    },
  };

  const result = tryDebugCompile(session, "main = 1");
  if (result.ok) {
    throw new Error("Expected debug compile command to fail.");
  }
  if (result.error !== "unsupported command") {
    throw new Error(`Unexpected compile error: ${result.error}`);
  }
});
