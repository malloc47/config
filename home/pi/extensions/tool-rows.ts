// Claude Code-style tool rows for every tool: "⏺ Read(src/a.ts)" with "⎿ Read 120 lines" under it,
// collapsed to a line or a short preview, and the tool's own full rendering on expand (ctrl+o).
//
// Display only: one tool renderer frames what the tool, or an extension loaded after this one, would
// draw. pi chains renderers in load order with the first loaded outermost, so a package that
// supplies content renderers (majordomo's summaries of its tools) loads after this one. It never
// registers a tool: majordomo's guard blocks re-registered built-ins.

import { homedir } from "node:os";

import type { ExtensionAPI, Theme, ToolRenderers } from "@earendil-works/pi-coding-agent";
import { type Component, Container, Text, truncateToWidth, visibleWidth } from "@earendil-works/pi-tui";

import { callArgs, callTitle, collapsed, duration, isBuiltIn, linesOf, mcpName, resultText, withoutNotes } from "../lib/rows.ts";

type RenderCall = NonNullable<ToolRenderers["renderCall"]>;
type RenderResult = NonNullable<ToolRenderers["renderResult"]>;
type Context = Parameters<RenderCall>[2];

/** Keys in the row's shared renderer state for the inner renderer's last components, which it may reuse. */
const LAST_CALL = "pi-personal:lastCall";
const LAST_RESULT = "pi-personal:lastResult";

/** Lines without the blank ones a tool's renderer may start with. */
function trimStart(lines: string[]): string[] {
  let start = 0;
  while (start < lines.length && visibleWidth(lines[start].replace(/\s+/g, "")) === 0) start++;
  return start === 0 ? lines : lines.slice(start);
}

/** `inner` drawn narrower, after a prefix on its first line and another on the rest. */
class Prefixed implements Component {
  private readonly inner: Component;
  private readonly first: string;
  private readonly rest: string;

  constructor(inner: Component, first: string, rest: string) {
    this.inner = inner;
    this.first = first;
    this.rest = rest;
  }

  render(width: number): string[] {
    const indent = visibleWidth(this.first);
    const lines = trimStart(this.inner.render(Math.max(1, width - indent)));
    return lines.map((line, i) => truncateToWidth((i === 0 ? this.first : this.rest) + line, width));
  }

  invalidate(): void {
    this.inner.invalidate();
  }
}

/** The first `max` lines of `inner`, then a line saying how many were left out. */
class Capped implements Component {
  private readonly inner: Component;
  private readonly max: number;
  private readonly hint: (hidden: number) => string;

  constructor(inner: Component, max: number, hint: (hidden: number) => string) {
    this.inner = inner;
    this.max = max;
    this.hint = hint;
  }

  render(width: number): string[] {
    const lines = trimStart(this.inner.render(width));
    if (lines.length <= this.max) return lines;
    return [...lines.slice(0, this.max), truncateToWidth(this.hint(lines.length - this.max), width)];
  }

  invalidate(): void {
    this.inner.invalidate();
  }
}

const home = process.env.HOME || homedir();

function bullet(theme: Theme, context: Context): string {
  if (context.isPartial) return theme.fg("muted", "⏺");
  return theme.fg(context.isError ? "error" : "success", "⏺");
}

/** "Read(src/a.ts)", with the duration when it took a while. */
function callLine(toolName: string, args: Record<string, unknown>, theme: Theme, context: Context): string {
  const inside = callArgs(toolName, args ?? {}, context.cwd, home);
  const mcp = mcpName(toolName);
  let line = theme.bold(callTitle(toolName));
  if (inside || !mcp) line += theme.fg("muted", "(") + inside + theme.fg("muted", ")");
  if (context.durationMs !== undefined && context.durationMs >= 2000) line += theme.fg("muted", ` · ${duration(context.durationMs)}`);
  return line;
}

function frame(toolName: string, inner: ToolRenderers | undefined): ToolRenderers {
  const ownCall = isBuiltIn(toolName) ? undefined : inner?.renderCall;
  const innerResult = inner?.renderResult;

  const renderCall: RenderCall = (args, theme, context) => {
    const pad = " ".repeat(context.outputPad);
    const state = context.state as Record<string, Component | undefined>;
    let body: Component;
    if (ownCall) {
      body = ownCall(args, theme, { ...context, lastComponent: state[LAST_CALL] });
      state[LAST_CALL] = body;
    } else {
      body = new Text(callLine(toolName, args as Record<string, unknown>, theme, context), 0, 0);
    }
    return new Prefixed(body, `${pad}${bullet(theme, context)} `, `${pad}  `);
  };

  const renderResult: RenderResult = (result, options, theme, context) => {
    const pad = " ".repeat(context.outputPad);
    const gutter = (component: Component) => new Prefixed(component, `${pad}  ${theme.fg("muted", "⎿")}  `, `${pad}     `);
    const state = context.state as Record<string, Component | undefined>;
    const renderInner = (): Component => {
      const component = (innerResult as RenderResult)(result, options, theme, { ...context, lastComponent: state[LAST_RESULT] });
      state[LAST_RESULT] = component;
      return component;
    };
    const expandHint = () => context.keyHint("app.tools.expand", "to expand");
    const hint = (hidden: number) => `${theme.fg("muted", `… +${hidden} lines (`)}${expandHint()}${theme.fg("muted", ")")}`;
    const lines = (text: string, color: "toolOutput" | "error") => new Text(text.split("\n").map((l) => theme.fg(color, l)).join("\n"), 0, 0);
    const text = resultText(result.content as never);
    const body = linesOf(withoutNotes(text));

    if (options.expanded) return gutter(innerResult ? renderInner() : lines(text, "toolOutput"));
    if (options.isPartial) {
      if (body.length === 0) return gutter(new Text(theme.fg("muted", "Running…"), 0, 0));
      return gutter(lines(body.slice(-3).join("\n"), "toolOutput"));
    }
    if (context.isError) {
      const error = body.filter((line) => line.trim() !== "" && line !== "(no output)").join("\n") || "Error";
      return gutter(new Capped(lines(error, "error"), 3, hint));
    }
    // Images draw at their own size, and only the tool's renderer knows how.
    if (innerResult && result.content.some((part) => part.type === "image")) return gutter(renderInner());
    // Another extension's renderer is already the summary; keep it short.
    if (!isBuiltIn(toolName) && innerResult) return gutter(new Capped(renderInner(), 8, hint));

    const plan = collapsed(toolName, (context.args ?? {}) as Record<string, unknown>, text, result.details, context.cwd, home);
    const view = new Container();
    if (plan.head) {
      const more = plan.preview === "none" && body.length > 0 && toolName !== "bash";
      view.addChild(new Text(more ? `${plan.head} ${theme.fg("muted", "(")}${expandHint()}${theme.fg("muted", ")")}` : plan.head, 0, 0));
    }
    if (plan.preview === "text") {
      view.addChild(body.length <= plan.lines ? lines(body.join("\n"), "toolOutput") : new Capped(lines(body.join("\n"), "toolOutput"), plan.lines, hint));
    } else if (plan.preview === "inner" && innerResult) {
      view.addChild(new Capped(renderInner(), plan.lines, hint));
    }
    return gutter(view);
  };

  return { ...inner, renderShell: "self", renderCall, renderResult };
}

export default function toolRows(pi: ExtensionAPI): void {
  // A throw while loading would stop pi from starting, majordomo's desk included.
  try {
    pi.registerToolRenderer((toolName, next) => frame(toolName, next()));
  } catch (error) {
    console.error(`pi-personal: tool rows are off: ${String(error)}`);
  }
}
