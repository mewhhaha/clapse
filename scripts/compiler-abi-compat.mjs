function exportNamesFromInstance(instance) {
  return Object.keys(instance?.exports ?? {});
}

export function preferredCompilerRunExportName(exportNames) {
  if (Array.isArray(exportNames) && exportNames.includes("clap_run")) {
    return "clap_run";
  }
  if (Array.isArray(exportNames) && exportNames.includes("clapse_run")) {
    return "clapse_run";
  }
  return null;
}

export function hasCompilerRunExport(exportNames) {
  return preferredCompilerRunExportName(exportNames) !== null;
}

export function compilerRunExportRequirementText() {
  return "clap_run (or legacy clapse_run during transition)";
}

export function getCompilerRunExport(instance, context = "compiler wasm") {
  const exportNames = exportNamesFromInstance(instance);
  const selected = preferredCompilerRunExportName(exportNames);
  if (selected === null) {
    throw new Error(
      `${context} export '${compilerRunExportRequirementText()}' missing (exports: ${exportNames.join(", ")})`,
    );
  }
  const fn = instance.exports[selected];
  if (typeof fn !== "function") {
    throw new Error(
      `${context} export '${selected}' is not callable (exports: ${exportNames.join(", ")})`,
    );
  }
  return fn;
}
