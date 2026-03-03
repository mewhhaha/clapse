import {
  runCompilePipeline,
  tryDebugCompile,
  compileDebugWithLoop,
} from "../static/compile_pipeline.js";

const compileDebugLoop = compileDebugWithLoop as (args: {
  session: { call(request: Record<string, unknown>): Record<string, unknown> };
  entryPath: string;
  moduleSources: Map<string, string>;
  explicitEntrypointExports?: string[];
}) => Record<string, unknown>;

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

Deno.test(
  "compileDebugWithLoop compiles with entrypoint_exports and tracks passes",
  () => {
    const calls: string[] = [];
    const session = {
      call(request: {
        command: string;
        compile_mode?: string;
        entrypoint_exports?: string[];
        input_path?: string;
      }) {
        calls.push(request.command);
        if (request.command === "compile") {
          calls.push(
            `entrypoint=${(request.entrypoint_exports ?? []).join(",")}`,
          );
          calls.push(`input=${request.input_path}`);
        }
        if (request.command !== "compile") {
          throw new Error(`Unexpected command: ${request.command}`);
        }

        if (calls.length <= 2) {
          return {
            ok: true,
            wasm_base64: "AA==",
            artifacts: {
              "lowered_ir.txt": "pass-1",
              "collapsed_ir.txt": "pass-1",
            },
          };
        }
        return {
          ok: true,
          wasm_base64: "AA==",
          artifacts: {
            "lowered_ir.txt": "pass-2",
            "collapsed_ir.txt": "pass-2",
          },
        };
      },
    };

    const result = compileDebugLoop({
      session,
      entryPath: "repl/input.clapse",
      moduleSources: new Map([
        ["repl/input.clapse", "export main\nmain = add 1 2"],
      ]),
      explicitEntrypointExports: ["main"],
    }) as {
      ok: boolean;
      passes: number;
      entryRoots: string[];
      usedEntrypointExports: boolean;
    };

    if (!result.ok) {
      throw new Error("Expected compile loop to succeed.");
    }
    if (result.passes !== 1) {
      throw new Error(`Expected one compile pass, got ${result.passes}`);
    }
    if (result.entryRoots.length !== 1 || result.entryRoots[0] !== "main") {
      throw new Error(`Unexpected entry roots: ${result.entryRoots.join(",")}`);
    }
    if (result.usedEntrypointExports !== true) {
      throw new Error("Expected entrypoint exports to be used.");
    }
    if (
      calls.join("|") !==
      ["compile", "entrypoint=main", "input=repl/input.clapse"].join("|")
    ) {
      throw new Error(`Unexpected compiler call order: ${calls.join(",")}`);
    }
  },
);

Deno.test(
  "compileDebugWithLoop falls back when entrypoint_exports unsupported",
  () => {
    const calls: string[] = [];
    let firstCompileWithExports = true;
    const session = {
      call(request: {
        command: string;
        entrypoint_exports?: string[];
        input_path?: string;
      }) {
        calls.push(request.command);
        if (request.command === "compile") {
          calls.push(
            "entrypoint=" + JSON.stringify(request.entrypoint_exports ?? null),
          );
        }
        if (request.command !== "compile") {
          throw new Error(`Unexpected command: ${request.command}`);
        }

        if (firstCompileWithExports && request.entrypoint_exports) {
          firstCompileWithExports = false;
          return { ok: false, error: "unknown field entrypoint_exports" };
        }
        return {
          ok: true,
          wasm_base64: "AA==",
          artifacts: {
            "lowered_ir.txt": "pass-fallback",
            "collapsed_ir.txt": "pass-fallback",
          },
        };
      },
    };

    const result = compileDebugLoop({
      session,
      entryPath: "repl/input.clapse",
      moduleSources: new Map([
        ["repl/input.clapse", "export main\nmain = add 1 2"],
      ]),
    }) as {
      ok: boolean;
      passes: number;
      usedEntrypointExports: boolean;
      entryRoots: string[];
    };

    if (!result.ok || result.usedEntrypointExports !== false) {
      throw new Error(
        "Expected compile loop to fallback to compile without entrypoint_exports.",
      );
    }

    const fallbackIndex = calls.indexOf("entrypoint=null");
    if (fallbackIndex < 0) {
      throw new Error("Expected fallback marker in call log.");
    }
  },
);

Deno.test("compileDebugWithLoop resolves imported modules", () => {
  const calls: string[] = [];
  const session = {
    call(request: {
      command: string;
      entrypoint_exports?: string[];
      input_path?: string;
    }) {
      calls.push(request.command);
      if (request.command !== "compile") {
        throw new Error(`Unexpected command: ${request.command}`);
      }
      return {
        ok: true,
        wasm_base64: "AA==",
        artifacts: {
          "lowered_ir.txt": "ok",
          "collapsed_ir.txt": "ok",
        },
      };
    },
  };

  const result = compileDebugLoop({
    session,
    entryPath: "src/main.clapse",
    moduleSources: new Map([
      ["src/main.clapse", 'import "sub"\nmain = sub()'],
      ["sub", "export sub\nsub = add 1 1"],
      ["/lib/util.clapse", "export add\nadd = ..."],
    ]),
    explicitEntrypointExports: ["main"],
  }) as {
    ok: boolean;
    passes: number;
    usedEntrypointExports: boolean;
    entryRoots: string[];
  };

  if (!result.ok) {
    throw new Error("Expected module import loop to succeed.");
  }
  if (!calls.includes("compile")) {
    throw new Error("Expected entry module to compile first.");
  }
  if (result.passes < 2) {
    throw new Error("Expected imported module to be discovered and compiled.");
  }
  if (result.usedEntrypointExports !== true) {
    throw new Error("Expected entrypoint exports to remain enabled.");
  }
});
