// A run of tool calls drawn as one line under one bullet, "⏺ Tools: Slack(read channel),
// Outlook(list mail messages)", in place of a row per call (lib/groups.ts says what a run is).
// Clicking an item opens that call's own row under the line, as tool-rows.ts draws it, and clicking
// it again closes it; clicking "Tools:" opens or closes them all. Clicking an open row's result
// shows its full output, as on any row. A call that fails opens on its own, and ctrl+o (pi's tool
// expansion) shows every call's row as usual.
//
// Which calls are grouped comes from the package's config.json (programs.pi-personal.toolGroups):
// globs over tool names to include and exclude (every MCP tool by default), and names for MCP
// servers.
//
// pi draws each call as a row of its own and has no API for grouping rows, so this leans on two
// things pi 1.5.2's rows do (modes/interactive/components/tool-execution.js):
// - a row drawn with renderShell "self" whose renderers draw no lines takes no space at all, not
//   even its blank line;
// - a click on a row goes to what its renderers drew before pi's own toggle of the row.
// So every grouped row draws a GroupLine that draws only in the run's first row, and the other rows
// draw nothing until they're opened.
//
// Display only. It loads first, so it wraps tool-rows.ts and any renderer loaded after it (such as
// majordomo's summaries of its sources' tools), and it never registers a tool: majordomo's guard
// blocks re-registered built-ins.

import { readFileSync } from "node:fs";
import { homedir } from "node:os";

import type { ExtensionAPI, ExtensionContext, ExtensionUIContext, Theme, ToolRenderers } from "@earendil-works/pi-coding-agent";
import { type Component, Container, Text, truncateToWidth, type TuiMouseEvent, type TuiMouseEventResult, visibleWidth } from "@earendil-works/pi-tui";

import { blocksOf, type CallState, configOf, DEFAULTS, grouped, itemAt, itemLabel, layout, type Placed, type Run, runsOf, runState, type ToolGroupsConfig } from "../lib/groups.ts";
import { type Args, resultText } from "../lib/rows.ts";

type RenderCall = NonNullable<ToolRenderers["renderCall"]>;
type RenderResult = NonNullable<ToolRenderers["renderResult"]>;
type Context = Parameters<RenderCall>[2];

let config: ToolGroupsConfig = DEFAULTS;
/** pi's UI, for whether ctrl+o has every row expanded; only a TUI session sets it. */
let ui: ExtensionUIContext | undefined;

/** What each grouped call last told its renderers, and how to have pi draw its row again. */
const calls = new Map<string, { name: string; args: Args; cwd: string; state: CallState; invalidate: () => void }>();
const home = process.env.HOME || homedir();
/** Calls whose own row is open under the line. */
const open = new Set<string>();

/** The session's messages so far, and the assistant message pi is streaming, if any. */
let committed: unknown[] = [];
let live: unknown;
let liveShape = "";
let runs = new Map<string, Run>();

function recompute(): void {
  runs = runsOf(blocksOf(live === undefined ? committed : [...committed, live]), (name) => grouped(config, name));
}

/** What about a streaming message can change its runs: its tool calls and whether it has text. */
function shapeOf(message: unknown): string {
  const content = (message as { content?: unknown }).content;
  if (!Array.isArray(content)) return "";
  return content.map((part: { type?: unknown; id?: unknown; text?: unknown }) => (part?.type === "toolCall" ? `c${String(part.id)}` : part?.type === "text" && typeof part.text === "string" && part.text.trim() ? "t" : "")).join(",");
}

function branchMessages(ctx: ExtensionContext): unknown[] {
  return ctx.sessionManager.getBranch().flatMap((entry): unknown[] => {
    if (entry.type === "message") return [entry.message];
    if (entry.type === "custom_message") return [{ role: "custom", display: entry.display }];
    return [];
  });
}

const everything = () => ui?.getToolsExpanded() === true;

function stateOf(context: Context): CallState {
  if (context.isPartial) return "pending";
  return context.isError ? "error" : "done";
}

/** Records what a call's renderers were told; a call that fails opens, and its run's line is drawn again. */
function note(toolName: string, context: Context): void {
  const id = context.toolCallId;
  const state = stateOf(context);
  const before = calls.get(id);
  calls.set(id, { name: toolName, args: (context.args ?? {}) as Args, cwd: context.cwd, state, invalidate: context.invalidate });
  if (state === "error" && before?.state !== "error") open.add(id);
  if (before && before.state !== state) {
    const anchor = runs.get(id)?.anchor;
    if (anchor !== undefined && anchor !== id) calls.get(anchor)?.invalidate();
  }
}

function setOpen(id: string, value: boolean): void {
  if (value) open.add(id);
  else open.delete(id);
  calls.get(id)?.invalidate();
}

function styled(text: string, state: CallState, opened: boolean, theme: Theme): string {
  const comma = text.endsWith(",") ? "," : "";
  let label = comma ? text.slice(0, -1) : text;
  if (opened) label = theme.underline(label);
  if (state === "pending") label = theme.fg("muted", label);
  else if (state === "error") label = theme.fg("error", label);
  return label + (comma ? theme.fg("muted", comma) : "");
}

/**
 * The run's line, drawn by every grouped row and showing only in the run's first: the calls a run
 * gains later, and those a session rebuild moves, show up at the next draw with no row drawn again.
 */
class GroupLine implements Component {
  private readonly id: string;
  private readonly theme: Theme;
  private readonly pad: number;
  private placed: Placed[] = [];
  private indent = 0;

  constructor(id: string, theme: Theme, pad: number) {
    this.id = id;
    this.theme = theme;
    this.pad = pad;
  }

  /** The run's calls when this row draws its line; a call not yet in any run is a run of its own. */
  private members(): string[] | undefined {
    const run = runs.get(this.id);
    if (!run) return [this.id];
    return run.anchor === this.id ? run.members : undefined;
  }

  render(width: number): string[] {
    this.placed = [];
    const members = this.members();
    if (!members) return [];
    const theme = this.theme;
    const states = members.map((id) => calls.get(id)?.state ?? "pending");
    const state = runState(states);
    const bullet = theme.fg(state === "pending" ? "muted" : state === "error" ? "error" : "success", "⏺");
    const prefix = `${" ".repeat(this.pad)}${bullet} ${theme.bold("Tools:")} `;
    this.indent = visibleWidth(prefix);
    const items = members.map((id) => {
      const call = calls.get(id);
      return { id, label: itemLabel(call?.name ?? "tool", call?.args ?? {}, config.labels, { cwd: call?.cwd ?? "", home }) };
    });
    this.placed = layout(items, this.indent, width, visibleWidth, (text, columns) => truncateToWidth(text, columns, "…"));
    const lines: string[] = [];
    for (const p of this.placed) {
      const i = members.indexOf(p.id);
      const item = styled(p.text, states[i], open.has(p.id), theme);
      lines[p.line] = lines[p.line] === undefined ? (p.line === 0 ? prefix : " ".repeat(this.indent)) + item : `${lines[p.line]} ${item}`;
    }
    return lines.map((line) => truncateToWidth(line, width));
  }

  handleMouse(event: TuiMouseEvent): TuiMouseEventResult | undefined {
    if (event.type !== "click" || event.button !== "left") return undefined;
    const members = this.members();
    if (!members) return undefined;
    const hit = itemAt(this.placed, this.indent, event.y, event.x);
    if (hit === "all") {
      const opening = members.some((id) => !open.has(id));
      for (const id of members) setOpen(id, opening);
    } else if (hit !== undefined) {
      setOpen(hit, !open.has(hit));
    }
    // A click between items is still the line's, so pi doesn't toggle the first call's row.
    return { handled: true };
  }

  invalidate(): void {}
}

function grouping(toolName: string, inner: ToolRenderers | undefined): ToolRenderers {
  const innerCall: RenderCall = (args, theme, context) => (inner?.renderCall ? inner.renderCall(args, theme, context) : new Text(theme.bold(toolName), 0, 0));
  const innerResult: RenderResult = (result, options, theme, context) =>
    inner?.renderResult ? inner.renderResult(result, options, theme, context) : new Text(theme.fg("toolOutput", resultText(result.content as never)), 0, 0);

  return {
    ...inner,
    renderShell: "self",
    renderCall: (args, theme, context) => {
      note(toolName, context);
      if (everything()) return innerCall(args, theme, context);
      const view = new Container();
      view.addChild(new GroupLine(context.toolCallId, theme, context.outputPad));
      if (open.has(context.toolCallId)) view.addChild(innerCall(args, theme, context));
      return view;
    },
    renderResult: (result, options, theme, context) => {
      note(toolName, context);
      if (everything() || open.has(context.toolCallId)) return innerResult(result, options, theme, context);
      return new Container();
    },
  };
}

function readJson(path: URL): unknown {
  try {
    return JSON.parse(readFileSync(path, "utf8"));
  } catch {
    return undefined;
  }
}

export default function toolGroups(pi: ExtensionAPI): void {
  // A throw while loading would stop pi from starting, majordomo's desk included.
  try {
    let problems: string[];
    ({ config, problems } = configOf(readJson(new URL("../config.json", import.meta.url))));
    if (!config.enable) return;

    pi.registerToolRenderer((toolName, next) => {
      const inner = next();
      return grouped(config, toolName) ? grouping(toolName, inner) : inner;
    });

    const reload = (ctx: ExtensionContext) => {
      committed = branchMessages(ctx);
      live = undefined;
      liveShape = "";
      recompute();
    };
    pi.on("session_start", (_event, ctx) => {
      if (ctx.mode !== "tui") return;
      ui = ctx.ui;
      for (const problem of problems) ctx.ui.notify(`pi-personal: ${problem}`, "warning");
      calls.clear();
      open.clear();
      reload(ctx);
    });
    // Compaction and moving in the tree rebuild the chat from the session.
    pi.on("session_compact", (_event, ctx) => reload(ctx));
    pi.on("session_tree", (_event, ctx) => reload(ctx));
    pi.on("message_update", (event) => {
      if ((event.message as { role?: unknown }).role !== "assistant") return;
      live = event.message;
      const shape = shapeOf(event.message);
      if (shape === liveShape) return;
      liveShape = shape;
      recompute();
    });
    pi.on("message_end", (event) => {
      committed.push(event.message);
      if ((event.message as { role?: unknown }).role === "assistant") {
        live = undefined;
        liveShape = "";
      }
      recompute();
    });
  } catch (error) {
    console.error(`pi-personal: tool groups are off: ${String(error)}`);
  }
}
