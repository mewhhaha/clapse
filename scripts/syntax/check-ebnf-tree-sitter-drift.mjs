#!/usr/bin/env deno run

const DEFAULT_EBNF_PATH = "docs/clapse-language/references/grammar.ebnf";
const DEFAULT_TREE_SITTER_GRAMMAR_PATH = "tree-sitter-clapse/grammar.js";

const ALLOWED_TREE_SITTER_KEYWORD_EXTRAS = new Set([
  "in",
  "infix",
  "infixl",
  "infixr",
  "let",
]);

const OPERATOR_REGEX_COVERED_EBNF_TERMINALS = new Set([
  "+",
  "-",
]);

if (import.meta.main) {
  main().catch((error) => {
    console.error(error);
    Deno.exit(1);
  });
}

async function main() {
  const ebnfPath = Deno.args[0] ?? DEFAULT_EBNF_PATH;
  const treeSitterGrammarPath = Deno.args[1] ?? DEFAULT_TREE_SITTER_GRAMMAR_PATH;

  const [ebnfText, treeSitterGrammarText] = await Promise.all([
    Deno.readTextFile(ebnfPath),
    Deno.readTextFile(treeSitterGrammarPath),
  ]);

  const ebnfTerminals = extractEbnfTerminals(ebnfText);
  const ebnfKeywords = new Set(ebnfTerminals.filter(isKeywordToken));
  const ebnfFixedTerminals = new Set(
    ebnfTerminals
      .filter((token) => !isKeywordToken(token))
      .filter((token) => !OPERATOR_REGEX_COVERED_EBNF_TERMINALS.has(token)),
  );

  const treeSitterKeywords = extractTreeSitterKeywords(treeSitterGrammarText);
  const missingTreeSitterKeywords = sortedSetDiff(ebnfKeywords, treeSitterKeywords);
  const unexpectedTreeSitterKeywords = sorted(
    [...setDiff(treeSitterKeywords, ebnfKeywords)].filter(
      (keyword) => !ALLOWED_TREE_SITTER_KEYWORD_EXTRAS.has(keyword),
    ),
  );

  const missingTreeSitterFixedTerminals = sorted(
    [...ebnfFixedTerminals].filter(
      (terminal) => !hasQuotedLiteral(treeSitterGrammarText, terminal),
    ),
  );

  if (
    missingTreeSitterKeywords.length > 0 ||
    unexpectedTreeSitterKeywords.length > 0 ||
    missingTreeSitterFixedTerminals.length > 0
  ) {
    const details = {
      missing_tree_sitter_keywords: missingTreeSitterKeywords,
      unexpected_tree_sitter_keywords: unexpectedTreeSitterKeywords,
      missing_tree_sitter_fixed_terminals: missingTreeSitterFixedTerminals,
      allowed_tree_sitter_keyword_extras: sorted([...ALLOWED_TREE_SITTER_KEYWORD_EXTRAS]),
      operator_regex_covered_ebnf_terminals: sorted([
        ...OPERATOR_REGEX_COVERED_EBNF_TERMINALS,
      ]),
      inputs: {
        ebnf: ebnfPath,
        tree_sitter_grammar: treeSitterGrammarPath,
      },
    };
    throw new Error(
      `ebnf-tree-sitter-drift-check: detected drift\n${JSON.stringify(details, null, 2)}`,
    );
  }

  console.log(
    `ebnf-tree-sitter-drift-check: PASS (keywords=${treeSitterKeywords.size}, fixed_terminals=${ebnfFixedTerminals.size})`,
  );
}

function extractEbnfTerminals(ebnfText) {
  const textWithoutComments = ebnfText.replace(/#.*$/gmu, "");
  const terminals = new Set();
  const pattern = /"([^"\\]*(?:\\.[^"\\]*)*)"/gmu;
  let match = pattern.exec(textWithoutComments);
  while (match !== null) {
    const token = decodeEscapedString(match[1]);
    if (isRelevantEbnfTerminal(token)) {
      terminals.add(token);
    }
    match = pattern.exec(textWithoutComments);
  }
  return sorted([...terminals]);
}

function decodeEscapedString(raw) {
  try {
    return JSON.parse(`"${raw}"`);
  } catch {
    return raw;
  }
}

function isRelevantEbnfTerminal(token) {
  if (token.length === 0) return false;
  if (/\s/u.test(token)) return false;
  if (token === "'" || token === '"') return false;
  return true;
}

function isKeywordToken(token) {
  return /^[a-z][a-z0-9_]*$/u.test(token);
}

function extractTreeSitterKeywords(grammarJs) {
  const keywords = new Set();
  const kwPattern = /_kw_[A-Za-z0-9_]+\s*:\s*\(\)\s*=>\s*token\(\s*prec\(\s*\d+\s*,\s*"([^"]+)"\s*\)\s*\)/gmu;
  let match = kwPattern.exec(grammarJs);
  while (match !== null) {
    keywords.add(match[1]);
    match = kwPattern.exec(grammarJs);
  }
  return keywords;
}

function hasQuotedLiteral(sourceText, token) {
  const escaped = escapeRegExp(token);
  const quotedPattern = new RegExp(`"(${escaped})"`, "u");
  return quotedPattern.test(sourceText);
}

function escapeRegExp(text) {
  return text.replace(/[.*+?^${}()|[\]\\]/g, "\\$&");
}

function setDiff(left, right) {
  const result = new Set();
  for (const value of left) {
    if (!right.has(value)) {
      result.add(value);
    }
  }
  return result;
}

function sortedSetDiff(left, right) {
  return sorted([...setDiff(left, right)]);
}

function sorted(values) {
  return [...values].sort((a, b) => {
    if (a < b) return -1;
    if (a > b) return 1;
    return 0;
  });
}
