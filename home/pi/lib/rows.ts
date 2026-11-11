// What a Claude Code-style tool row says: the title on the call line ("Read(src/a.ts)") and the
// summary under it ("Read 120 lines"). Pure functions of the tool's name, arguments, and result, so
// they're tested without pi; extensions/tool-rows.ts draws them.

export type Args = Record<string, unknown>;

/** Claude Code's names for pi's built-in tools. */
const TITLES: Record<string, string> = {
  read: "Read",
  bash: "Bash",
  edit: "Update",
  write: "Write",
  grep: "Search",
  find: "Find",
  ls: "List",
};

export function isBuiltIn(toolName: string): boolean {
  return toolName in TITLES;
}

/** An MCP tool's server and tool, from pi's `mcp__<server>__<tool>` names. */
export function mcpName(toolName: string): { server: string; tool: string } | undefined {
  const match = /^mcp__(.+?)__(.+)$/.exec(toolName);
  return match ? { server: match[1], tool: match[2] } : undefined;
}

/** A path as the principal would type it: relative to the working directory, or under ~. */
export function shortPath(path: string, cwd: string, home: string | undefined): string {
  if (cwd && path.startsWith(`${cwd}/`)) return path.slice(cwd.length + 1);
  if (home && (path === home || path.startsWith(`${home}/`))) return `~${path.slice(home.length)}`;
  return path;
}

/** Text on one line, cut to `max` columns with an ellipsis. */
export function oneLine(text: string, max: number): string {
  const flat = text.replace(/\s+/g, " ").trim();
  return flat.length > max ? `${flat.slice(0, Math.max(0, max - 1))}…` : flat;
}

function brief(value: unknown, max: number): string {
  if (typeof value === "string") return JSON.stringify(oneLine(value, max));
  const json = JSON.stringify(value) ?? String(value);
  return oneLine(json, max);
}

/** The call line's title: Claude Code's name for a built-in, `server - tool (MCP)`, or the tool's name. */
export function callTitle(toolName: string): string {
  const mcp = mcpName(toolName);
  return TITLES[toolName] ?? (mcp ? `${mcp.server} - ${mcp.tool} (MCP)` : toolName);
}

/** What goes in the call line's parentheses: the main argument for built-ins, else the first few `key: value` pairs. */
export function callArgs(toolName: string, args: Args, cwd: string, home: string | undefined): string {
  const path = (key = "path") => (typeof args[key] === "string" ? shortPath(args[key] as string, cwd, home) : "");
  switch (toolName) {
    case "read": {
      const offset = typeof args.offset === "number" ? args.offset : undefined;
      const limit = typeof args.limit === "number" ? args.limit : undefined;
      if (offset === undefined && limit === undefined) return path();
      const from = offset ?? 1;
      return `${path()} · lines ${from}–${limit === undefined ? "" : from + limit - 1}`;
    }
    case "bash":
      return typeof args.command === "string" ? oneLine(args.command, 200) : "";
    case "edit":
    case "write":
      return path();
    case "grep":
    case "find": {
      const parts = [`pattern: ${brief(args.pattern ?? "", 80)}`];
      if (typeof args.path === "string" && args.path !== "." && args.path !== "") parts.push(`path: ${JSON.stringify(path())}`);
      if (typeof args.glob === "string") parts.push(`glob: ${JSON.stringify(args.glob)}`);
      return parts.join(", ");
    }
    case "ls":
      return path() || ".";
  }
  const entries = Object.entries(args ?? {}).filter(([, value]) => value !== undefined);
  const shown = entries.slice(0, 3).map(([key, value]) => `${key}: ${brief(value, 60)}`);
  return entries.length > shown.length ? `${shown.join(", ")}, …` : shown.join(", ");
}

/** A result's text parts, joined. */
export function resultText(content: readonly { type: string; text?: string }[] | undefined): string {
  return (content ?? [])
    .filter((part) => part.type === "text" && typeof part.text === "string")
    .map((part) => part.text as string)
    .join("\n");
}

/** Text without the bracketed notes pi appends after a blank line ("[Showing lines 1-20 of 90. …]"). */
export function withoutNotes(text: string): string {
  return text.replace(/(\n\n\[[^\n]*\])+\s*$/, "");
}

/** Text as lines, without trailing blank lines. */
export function linesOf(text: string): string[] {
  const lines = text.replace(/\s+$/, "").split("\n");
  return lines.length === 1 && lines[0] === "" ? [] : lines;
}

function count(n: number, one: string, many = `${one}s`): string {
  return `${n} ${n === 1 ? one : many}`;
}

/** Additions and removals in pi's display diff (lines starting `+` or `-`, then a line number). */
export function diffStats(diff: string): { added: number; removed: number } {
  let added = 0;
  let removed = 0;
  for (const line of diff.split("\n")) {
    if (/^\+\s*\d/.test(line)) added++;
    else if (/^-\s*\d/.test(line)) removed++;
  }
  return { added, removed };
}

/**
 * How a finished result reads collapsed. `head` is a sentence ("Read 120 lines"); `preview` says what
 * follows it: output lines (`text`, at most `lines` of them), the tool's own rendering (`inner`, cut
 * to `lines`), or nothing.
 */
export type Collapsed = {
  head?: string;
  preview: "text" | "inner" | "none";
  lines: number;
};

export function collapsed(toolName: string, args: Args, text: string, details: unknown, cwd: string, home: string | undefined): Collapsed {
  const body = linesOf(withoutNotes(text));
  switch (toolName) {
    case "read":
      return { head: `Read ${count(body.length, "line")}`, preview: "none", lines: 0 };
    case "bash":
      return body.length === 0 ? { head: "(No content)", preview: "none", lines: 0 } : { preview: "text", lines: 3 };
    case "edit": {
      const diff = typeof (details as { diff?: unknown })?.diff === "string" ? (details as { diff: string }).diff : "";
      const { added, removed } = diffStats(diff);
      const target = typeof args.path === "string" ? shortPath(args.path, cwd, home) : "the file";
      return { head: `Updated ${target} with ${count(added, "addition")} and ${count(removed, "removal")}`, preview: "inner", lines: 10 };
    }
    case "write": {
      const written = typeof args.content === "string" ? linesOf(args.content).length : 0;
      const target = typeof args.path === "string" ? shortPath(args.path, cwd, home) : "the file";
      return { head: `Wrote ${count(written, "line")} to ${target}`, preview: "none", lines: 0 };
    }
    case "grep": {
      // Matches are `path:N: text`; context lines are `path-N- text`.
      const matches = body.filter((line) => /^[^[].*:\d+: /.test(line)).length;
      return { head: matches === 0 ? "No matches" : `Found ${count(matches, "line")}`, preview: "none", lines: 0 };
    }
    case "find":
      return { head: body.length === 0 ? "No files found" : `Found ${count(body.filter((l) => !l.startsWith("[")).length, "file")}`, preview: "none", lines: 0 };
    case "ls":
      return { head: `Listed ${count(body.filter((l) => !l.startsWith("[")).length, "path")}`, preview: "none", lines: 0 };
  }
  return body.length === 0 ? { head: "(No content)", preview: "none", lines: 0 } : { preview: "text", lines: 3 };
}

/** `4.2s` below a minute, then `1m 30s`. */
export function duration(ms: number): string {
  const seconds = ms / 1000;
  if (seconds < 60) return `${seconds.toFixed(1)}s`;
  return `${Math.floor(seconds / 60)}m ${Math.floor(seconds % 60)}s`;
}
