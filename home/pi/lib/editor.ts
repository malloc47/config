// The prompt editor's Claude Code keys: ctrl+l clears the prompt, a backslash before Enter makes a
// newline, and ctrl+r searches the prompts sent before. Pure, so it's tested without pi;
// extensions/chrome.ts and extensions/history.ts bind it to pi's editor and sessions.

import { validKey } from "./keys.ts";

export type EditorConfig = {
  keys: { clear: string; history: string };
  /** A backslash right before the cursor turns Enter into a newline, as in Claude Code. */
  backslashNewline: boolean;
  /** Past prompts the history search keeps, newest first. */
  historyLimit: number;
};

export const DEFAULTS: EditorConfig = { keys: { clear: "ctrl+l", history: "ctrl+r" }, backslashNewline: true, historyLimit: 1000 };

/** The configuration from the package's config.json, each bad value replaced by its default and named in `problems`. */
export function configOf(json: unknown): { config: EditorConfig; problems: string[] } {
  const given = (json as { editor?: Record<string, unknown> } | undefined)?.editor ?? {};
  const problems: string[] = [];
  const keysGiven = (given.keys ?? {}) as Record<string, unknown>;
  const key = (name: "clear" | "history") => {
    const value = keysGiven[name];
    if (value === undefined) return DEFAULTS.keys[name];
    if (typeof value === "string" && validKey(value)) return value.toLowerCase();
    problems.push(`editor.keys.${name} (${JSON.stringify(value)}) isn't a key; using ${DEFAULTS.keys[name]}`);
    return DEFAULTS.keys[name];
  };
  const keys = { clear: key("clear"), history: key("history") };
  if (keys.clear === keys.history) {
    problems.push(`editor.keys.clear and history are both ${keys.clear}; using the defaults`);
    Object.assign(keys, DEFAULTS.keys);
  }
  let historyLimit = DEFAULTS.historyLimit;
  if (given.historyLimit !== undefined) {
    if (Number.isInteger(given.historyLimit) && (given.historyLimit as number) > 0) historyLimit = given.historyLimit as number;
    else problems.push(`editor.historyLimit (${JSON.stringify(given.historyLimit)}) isn't a positive whole number; using ${DEFAULTS.historyLimit}`);
  }
  const backslashNewline = typeof given.backslashNewline === "boolean" ? given.backslashNewline : DEFAULTS.backslashNewline;
  return { config: { keys, historyLimit, backslashNewline }, problems };
}

/** Whether the character just before the cursor is a backslash. */
export function backslashBeforeCursor(lines: readonly string[], cursor: { line: number; col: number }): boolean {
  const line = lines[cursor.line] ?? "";
  return cursor.col > 0 && line[cursor.col - 1] === "\\";
}

export type Prompt = { text: string; at: string; cwd: string };

type Entry = { type?: string; timestamp?: string; message?: { role?: string; content?: unknown; timestamp?: number } };

/**
 * The prompts a session's entries hold, oldest first. Messages that are envelopes other programs
 * wrote rather than prompts someone typed (`<routine …>`, `<inbound …>`) are left out.
 */
export function promptsOf(entries: readonly Entry[], cwd: string): Prompt[] {
  const prompts: Prompt[] = [];
  for (const entry of entries) {
    if (entry.type !== "message" || entry.message?.role !== "user") continue;
    const content = entry.message.content;
    const text = (
      typeof content === "string"
        ? content
        : Array.isArray(content)
          ? content.map((part: { type?: string; text?: string }) => (part?.type === "text" ? (part.text ?? "") : "")).join("")
          : ""
    ).trim();
    // An envelope is one whole element, from its opening tag to its closing one.
    if (!text || /^<([a-z][\w-]*)[\s>][\s\S]*<\/\1>$/i.test(text) || /^<[a-z][\w-]*\s[^>]*\/>$/i.test(text)) continue;
    const at = entry.timestamp ?? (typeof entry.message.timestamp === "number" ? new Date(entry.message.timestamp).toISOString() : "");
    prompts.push({ text, at, cwd });
  }
  return prompts;
}

/** Prompts newest first, each text once (at its newest use), at most `limit` of them. */
export function newestFirst(prompts: readonly Prompt[], limit: number): Prompt[] {
  const sorted = [...prompts].sort((a, b) => (a.at < b.at ? 1 : a.at > b.at ? -1 : 0));
  const seen = new Set<string>();
  const out: Prompt[] = [];
  for (const p of sorted) {
    if (seen.has(p.text)) continue;
    seen.add(p.text);
    out.push(p);
    if (out.length >= limit) break;
  }
  return out;
}

/** The prompts holding every word of the query, in any order and any case, newest first. */
export function matching(prompts: readonly Prompt[], query: string): Prompt[] {
  const words = query.toLowerCase().split(/\s+/).filter(Boolean);
  if (words.length === 0) return [...prompts];
  return prompts.filter((p) => {
    const text = p.text.toLowerCase();
    return words.every((w) => text.includes(w));
  });
}
