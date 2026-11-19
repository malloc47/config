// Tool groups: a run of tool calls drawn as one line, "⏺ Tools: Slack(read channel), Outlook(list
// mail messages)", where clicking an item opens that call's own row. Pure, so it's tested without
// pi; extensions/tool-groups.ts draws it.
//
// A run is the calls with nothing visible between them: no text from the model, no message of
// yours, and no row of a tool that isn't grouped. Thinking doesn't end a run, since a model that
// thinks before each call would otherwise get a run per call.

import { type Args, callArgs, callTitle, isBuiltIn, mcpName, oneLine } from "./rows.ts";

export type ToolGroupsConfig = {
  enable: boolean;
  /** Globs over pi's tool names whose calls are grouped; `*` matches any run of characters. */
  include: string[];
  /** Globs never grouped; they win over `include`. */
  exclude: string[];
  /** Names to show for MCP servers, by the server's name in pi's `mcp__<server>__<tool>` tool names. */
  labels: Record<string, string>;
};

export const DEFAULTS: ToolGroupsConfig = { enable: true, include: ["mcp__*"], exclude: [], labels: {} };

/** The longest an item's label gets, in columns. */
export const MAX_LABEL = 40;

function globs(value: unknown): string[] | undefined {
  return Array.isArray(value) && value.every((v) => typeof v === "string" && v !== "") ? (value as string[]) : undefined;
}

/** The configuration from the package's config.json, each bad value replaced by its default and named in `problems`. */
export function configOf(json: unknown): { config: ToolGroupsConfig; problems: string[] } {
  const given = (json as { toolGroups?: Record<string, unknown> } | undefined)?.toolGroups ?? {};
  const problems: string[] = [];
  const enable = typeof given.enable === "boolean" ? given.enable : DEFAULTS.enable;
  const list = (name: "include" | "exclude") => {
    if (given[name] === undefined) return DEFAULTS[name];
    const value = globs(given[name]);
    if (value) return value;
    problems.push(`toolGroups.${name} (${JSON.stringify(given[name])}) isn't a list of tool-name globs; using ${JSON.stringify(DEFAULTS[name])}`);
    return DEFAULTS[name];
  };
  let labels = DEFAULTS.labels;
  if (given.labels !== undefined) {
    const value = given.labels;
    if (value !== null && typeof value === "object" && !Array.isArray(value) && Object.values(value).every((v) => typeof v === "string" && v.trim() !== "")) {
      labels = value as Record<string, string>;
    } else {
      problems.push(`toolGroups.labels (${JSON.stringify(value)}) isn't a map of server names to labels; using none`);
    }
  }
  return { config: { enable, include: list("include"), exclude: list("exclude"), labels }, problems };
}

/** Whether `name` matches `pattern`, where `*` matches any run of characters and everything else matches itself. */
export function globMatch(pattern: string, name: string): boolean {
  const source = pattern
    .split("*")
    .map((part) => part.replace(/[.+?^${}()|[\]\\]/g, "\\$&"))
    .join(".*");
  return new RegExp(`^${source}$`, "s").test(name);
}

/** Whether calls to `toolName` are grouped. */
export function grouped(config: ToolGroupsConfig, toolName: string): boolean {
  return config.enable && config.include.some((p) => globMatch(p, toolName)) && !config.exclude.some((p) => globMatch(p, toolName));
}

/** What the chat shows, in order, as far as runs go: a tool call's row, or something between rows that ends a run. */
export type Block = { kind: "tool"; id: string; name: string } | { kind: "break" };

const BREAK: Block = { kind: "break" };

/**
 * The blocks of messages as pi's chat shows them. An assistant message shows its text and one row
 * per tool call, in order, and its thinking as at most a label; a tool result fills its call's row;
 * a custom message shows only when it's for display; anything else (your messages, ! commands,
 * summaries) is a row of its own.
 */
export function blocksOf(messages: readonly unknown[]): Block[] {
  const blocks: Block[] = [];
  for (const message of messages) {
    if (message === null || typeof message !== "object") continue;
    const { role, content, display } = message as { role?: unknown; content?: unknown; display?: unknown };
    if (role === "assistant") {
      if (!Array.isArray(content)) continue;
      for (const part of content as { type?: unknown; text?: unknown; id?: unknown; name?: unknown }[]) {
        if (part?.type === "text" && typeof part.text === "string" && part.text.trim() !== "") blocks.push(BREAK);
        else if (part?.type === "toolCall" && typeof part.id === "string" && typeof part.name === "string") blocks.push({ kind: "tool", id: part.id, name: part.name });
      }
    } else if (role === "toolResult") {
      continue;
    } else if (role === "custom") {
      if (display === true) blocks.push(BREAK);
    } else {
      blocks.push(BREAK);
    }
  }
  return blocks;
}

/** A run of grouped calls: the call whose row draws the line, and every call in the run, in order. */
export type Run = { anchor: string; members: string[] };

/** Each grouped call's run, by its id. A call seen twice keeps its first place. */
export function runsOf(blocks: readonly Block[], isGrouped: (toolName: string) => boolean): Map<string, Run> {
  const runs = new Map<string, Run>();
  let run: Run | undefined;
  for (const block of blocks) {
    if (block.kind === "tool" && runs.has(block.id)) continue;
    if (block.kind === "tool" && isGrouped(block.name)) {
      run ??= { anchor: block.id, members: [] };
      run.members.push(block.id);
      runs.set(block.id, run);
    } else {
      run = undefined;
    }
  }
  return runs;
}

function titled(word: string): string {
  return word.charAt(0).toUpperCase() + word.slice(1);
}

/** Where a call ran, for its label's paths: the working directory and the home directory. */
export type Where = { cwd: string; home: string | undefined };

/** `title(inside)` in at most `max` columns, cutting what's inside the parentheses first; just the title when there's nothing inside. */
function titledCall(title: string, inside: string, max: number): string {
  const name = oneLine(title, Math.max(4, max - 6));
  const flat = oneLine(inside, 1000);
  return flat ? `${name}(${oneLine(flat, Math.max(1, max - name.length - 2))})` : name;
}

/**
 * An item's label, cut to `max` columns:
 * - an MCP tool: "Slack(read channel)", from the server's label (or its name, capitalized) and the
 *   tool's name in words, without a repeat of the server's name in front;
 * - one of pi's built-ins: as its row's call line reads, "Read(USER.md)" or "Update(LOOPS.md)";
 * - any other tool: its name, capitalized ("Journal").
 */
export function itemLabel(toolName: string, args: Args, labels: Readonly<Record<string, string>>, where: Where, max = MAX_LABEL): string {
  if (isBuiltIn(toolName)) return titledCall(callTitle(toolName), callArgs(toolName, args ?? {}, where.cwd, where.home), max);
  const mcp = mcpName(toolName);
  if (!mcp) return oneLine(titled(toolName), max);
  const server = oneLine(labels[mcp.server] ?? titled(mcp.server), Math.max(4, max - 6));
  const repeat = `${mcp.server}_`;
  const tool = mcp.tool.toLowerCase().startsWith(repeat.toLowerCase()) && mcp.tool.length > repeat.length ? mcp.tool.slice(repeat.length) : mcp.tool;
  const words = tool.replace(/[_-]+/g, " ").trim() || tool;
  return `${server}(${oneLine(words, Math.max(1, max - server.length - 2))})`;
}

export type CallState = "pending" | "done" | "error";

/** A run's state for its bullet: pending while any call runs, else an error if any failed. */
export function runState(states: readonly CallState[]): CallState {
  if (states.includes("pending")) return "pending";
  return states.includes("error") ? "error" : "done";
}

/** An item where the line draws it: on line `line`, from column `from` up to `to`, as `text`. */
export type Placed = { id: string; text: string; line: number; from: number; to: number };

/**
 * Lays labels out after a prefix `indent` columns wide, in lines of `width` columns: items
 * separated by ", ", wrapping only between items, each later line indented as far as the first's
 * prefix. A label too wide for a line of its own is cut to fit. `widthOf` and `cut` measure and
 * cut styled-free text in terminal columns.
 */
export function layout(
  items: readonly { id: string; label: string }[],
  indent: number,
  width: number,
  widthOf: (text: string) => number = (text) => text.length,
  cut: (text: string, columns: number) => string = (text, columns) => oneLine(text, columns),
): Placed[] {
  const placed: Placed[] = [];
  const room = Math.max(1, width - indent);
  let line = 0;
  let column = indent;
  let first = true;
  items.forEach((item, i) => {
    const comma = i < items.length - 1 ? "," : "";
    let text = item.label + comma;
    const gap = first ? 0 : 1;
    if (!first && column + gap + widthOf(text) > width) {
      line++;
      column = indent;
      first = true;
    }
    const start = column + (first ? 0 : 1);
    if (widthOf(text) > room) text = cut(item.label, Math.max(1, room - widthOf(comma))) + comma;
    const end = start + widthOf(text);
    placed.push({ id: item.id, text, line, from: start, to: end });
    column = end;
    first = false;
  });
  return placed;
}

/** What a click at column `x` of line `y` hits: an item's id, "all" for the prefix, or nothing. */
export function itemAt(placed: readonly Placed[], indent: number, y: number, x: number): string | "all" | undefined {
  if (y === 0 && x < indent) return "all";
  return placed.find((p) => p.line === y && x >= p.from && x < p.to)?.id;
}
