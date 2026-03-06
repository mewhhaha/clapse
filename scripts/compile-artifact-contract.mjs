export function hasSyntheticArtifactMarkers(value) {
  if (typeof value !== "string") {
    return true;
  }
  return value.includes("kernel:compile:") ||
    /seed-stage[0-9]+:[^)\s"]+/u.test(value);
}

function assert(condition, message) {
  if (!condition) {
    throw new Error(message);
  }
}

function hasHeader(text, label) {
  return typeof text === "string" &&
    text.startsWith(`(${label})\n`) &&
    text.includes("kind: ");
}

function hasDef(text, name) {
  if (typeof text !== "string" || typeof name !== "string" || name.length === 0) {
    return false;
  }
  const escaped = name.replace(/[.*+?^${}()|[\]\\]/gu, "\\$&");
  const pattern = new RegExp(`(^|\\n)\\s*${escaped}(?=\\s|=)`, "u");
  return pattern.test(text);
}

export function assertStructuralArtifacts(
  lowered,
  collapsed,
  {
    context,
    requiredDefs = [],
    forbiddenDefs = [],
  },
) {
  assert(
    typeof lowered === "string" && lowered.length > 0,
    `${context}: lowered_ir.txt should be non-empty`,
  );
  assert(
    typeof collapsed === "string" && collapsed.length > 0,
    `${context}: collapsed_ir.txt should be non-empty`,
  );
  assert(
    !hasSyntheticArtifactMarkers(lowered),
    `${context}: lowered_ir.txt should not contain synthetic markers`,
  );
  assert(
    !hasSyntheticArtifactMarkers(collapsed),
    `${context}: collapsed_ir.txt should not contain synthetic markers`,
  );
  assert(
    hasHeader(lowered, "lowered_ir"),
    `${context}: lowered_ir.txt should use structural lowered_ir format`,
  );
  assert(
    hasHeader(collapsed, "collapsed_ir"),
    `${context}: collapsed_ir.txt should use structural collapsed_ir format`,
  );
  for (const name of requiredDefs) {
    assert(
      hasDef(lowered, name),
      `${context}: lowered_ir.txt missing required def '${name}'`,
    );
    assert(
      hasDef(collapsed, name),
      `${context}: collapsed_ir.txt missing required def '${name}'`,
    );
  }
  for (const name of forbiddenDefs) {
    assert(
      !hasDef(lowered, name),
      `${context}: lowered_ir.txt should not include def '${name}'`,
    );
    assert(
      !hasDef(collapsed, name),
      `${context}: collapsed_ir.txt should not include def '${name}'`,
    );
  }
}
