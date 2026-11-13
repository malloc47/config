// Claude Code-style chrome: a compact welcome header, a status-line footer, the "❯ " prompt, the ✻
// spinner with the run's elapsed time and the tokens it has produced, "✻ Thinking…" for collapsed
// thinking, and a terminal title that says when pi is working ("✳ name · working").
//
// It rings the terminal's bell when something needs you: a run that took a while has finished, or
// another extension says so on the "attention" event channel (majordomo, when it asks for an
// approval). The bell crosses tmux and mosh, which marks the window and passes it to your terminal.
//
// The prompt editor takes Claude Code's backslash before Enter as a newline (editor.backslashNewline
// in the package's config.json), and submits a prompt another extension sends on the
// "editor-submit" channel as if you'd typed it: pi.events.emit("editor-submit", request) with
// request.text, and request.handled set true when it went.
//
// Other extensions take part without replacing any of it: the footer shows every ctx.ui.setStatus
// text, and the spinner's message shows the label last sent on the "working-label" event channel
// (pi.events.emit("working-label", "Searching Slack"); undefined clears it). A later setHeader
// replaces this header, and a later setTitle the title, which is how a desk session shows its own.

import { readFileSync } from "node:fs";
import { basename } from "node:path";

import { CustomEditor, type ExtensionAPI, type ExtensionContext, type KeybindingsManager, keyText, VERSION } from "@earendil-works/pi-coding-agent";
import { truncateToWidth, visibleWidth } from "@earendil-works/pi-tui";

import { contextGauge, formatTokens, shortCwd, streamedTokens, usageTotals } from "../lib/chrome.ts";
import { backslashBeforeCursor, configOf as editorConfigOf } from "../lib/editor.ts";
import { PROMPT, PROMPT_WIDTH, withPrompt } from "../lib/prompt.ts";

const SPINNER = ["·", "✢", "✳", "✶", "✻", "✽", "✻", "✶", "✳", "✢"];
/** A run at least this long rings the bell when it finishes: long enough that you've looked away. */
const LONG_RUN_MS = 15_000;

/** The terminal's bell. It doesn't move the cursor, so it can't disturb pi's drawing. */
function bell(): void {
  process.stdout.write("\x07");
}
const VERBS = ["Thinking", "Pondering", "Working", "Considering", "Mulling", "Puzzling", "Brewing", "Musing"];

/** Left and right text on one line, the right truncated first. */
function spread(left: string, right: string, width: number): string {
  const gap = width - visibleWidth(left) - visibleWidth(right);
  if (gap >= 2) return left + " ".repeat(gap) + right;
  const room = width - visibleWidth(left) - 2;
  if (room > 8) return `${left}  ${truncateToWidth(right, room, "…")}`;
  return truncateToWidth(left, width, "…");
}

/**
 * pi's editor with the "❯ " prompt, drawn in left padding it keeps for it, in the editor's border
 * color. It embeds the working status in its border as pi's own editor does; pi carries the text,
 * callbacks, padding, and autocomplete over to it.
 */
class PromptEditor extends CustomEditor {
  private readonly keyMap: KeybindingsManager;

  constructor(...[tui, theme, keybindings]: ConstructorParameters<typeof CustomEditor>) {
    super(tui, theme, keybindings, { paddingX: PROMPT_WIDTH, embedWorkingStatus: true });
    this.keyMap = keybindings;
    activeEditor = this;
  }

  handleInput(data: string): void {
    // A backslash right before the cursor makes Enter a newline, and goes away, as in Claude Code.
    if (backslashNewline && this.keyMap.matches(data, "tui.input.submit") && !this.isShowingAutocomplete() && backslashBeforeCursor(this.getLines(), this.getCursor())) {
      super.handleInput("\x7f");
      this.insertTextAtCursor("\n");
      return;
    }
    super.handleInput(data);
  }

  setPaddingX(padding: number): void {
    super.setPaddingX(Math.max(PROMPT_WIDTH, padding));
  }

  render(width: number): string[] {
    return withPrompt(super.render(width), `${this.borderColor(PROMPT)} `);
  }
}

/** The prompt editor pi shows now, for prompts other extensions submit. */
let activeEditor: PromptEditor | undefined;
let backslashNewline = true;

function install(pi: ExtensionAPI, ctx: ExtensionContext): void {
  const home = process.env.HOME;

  ctx.ui.setHeader((_tui, theme) => ({
    render(width: number): string[] {
      const title = `${theme.fg("accent", "✻")} ${theme.bold("pi")} ${theme.fg("muted", VERSION)}`;
      const cwd = theme.fg("muted", shortCwd(ctx.cwd, home));
      const help = theme.fg("muted", "/hotkeys for shortcuts · /settings to configure · ctrl+o expands tool output");
      const inner = Math.max(visibleWidth(title), visibleWidth(cwd), visibleWidth(help));
      if (inner + 4 > width) return [title, cwd].map((line) => truncateToWidth(line, width, "…"));
      const row = (line: string) => `${theme.fg("borderAccent", "│")} ${line}${" ".repeat(inner - visibleWidth(line))} ${theme.fg("borderAccent", "│")}`;
      return [
        theme.fg("borderAccent", `╭${"─".repeat(inner + 2)}╮`),
        row(title),
        row(""),
        row(help),
        row(cwd),
        theme.fg("borderAccent", `╰${"─".repeat(inner + 2)}╯`),
      ];
    },
    invalidate() {},
  }));

  ctx.ui.setFooter((tui, theme, footerData) => {
    const unsubscribe = footerData.onBranchChange(() => tui.requestRender());
    let cached: { entries: number; totals: ReturnType<typeof usageTotals> } | undefined;
    return {
      dispose: unsubscribe,
      invalidate() {},
      render(width: number): string[] {
        const entries = ctx.sessionManager.getEntries();
        if (cached?.entries !== entries.length) cached = { entries: entries.length, totals: usageTotals(entries as never) };
        const { input, output, cost } = cached.totals;

        const model = ctx.model;
        const thinking = model?.reasoning ? pi.getThinkingLevel() : undefined;
        const modelPart = theme.fg("accent", model?.id ?? "no model") + (thinking ? theme.fg("muted", ` · ${thinking === "off" ? "thinking off" : thinking}`) : "");
        const branch = footerData.getGitBranch();
        const name = pi.getSessionName();
        const place = theme.fg("muted", shortCwd(ctx.sessionManager.getCwd(), home)) + (branch ? ` ${theme.fg("customMessageLabel", branch)}` : "") + (name ? theme.fg("muted", ` · ${name}`) : "");

        const usage = ctx.getContextUsage();
        const gauge = contextGauge(usage?.percent ?? undefined);
        const gaugeText = theme.fg(gauge.tone === "ok" ? "muted" : gauge.tone, `${gauge.bar} ${gauge.label}`);
        const stats = [input || output ? `↑${formatTokens(input)} ↓${formatTokens(output)}` : "", cost ? `$${cost.toFixed(2)}` : ""].filter(Boolean).join(" ");
        const right = gaugeText + (stats ? theme.fg("muted", ` · ${stats}`) : "");

        const lines = [spread(`${modelPart}${theme.fg("muted", " · ")}${place}`, right, width)];
        const statuses = [...footerData.getExtensionStatuses().entries()]
          .sort(([a], [b]) => a.localeCompare(b))
          .map(([, text]) => text.replace(/\s+/g, " ").trim())
          .filter(Boolean);
        if (statuses.length > 0) lines.push(truncateToWidth(statuses.join(theme.fg("muted", " · ")), width, "…"));
        return lines;
      },
    };
  });

  ctx.ui.setEditorComponent((tui, theme, keybindings) => new PromptEditor(tui, theme, keybindings));
  ctx.ui.setWorkingIndicator({ frames: SPINNER.map((frame) => ctx.ui.theme.fg("accent", frame)), intervalMs: 120 });
  ctx.ui.setHiddenThinkingLabel("✻ Thinking…");
  idleTitle(ctx);
}

/**
 * The idle title, "✻ <dir>". pi titles the terminal itself once a session's extensions are bound,
 * and again when the session's name changes, so it's set again just after pi's.
 */
function idleTitle(ctx: ExtensionContext): void {
  const title = `✻ ${basename(ctx.cwd)}`;
  ctx.ui.setTitle(title);
  for (const ms of [100, 1000]) {
    setTimeout(() => {
      try {
        ctx.ui.setTitle(title);
      } catch {
        // pi has replaced the session; its own session_start titles it.
      }
    }, ms).unref();
  }
}

export default function chrome(pi: ExtensionAPI): void {
  let label: string | undefined;
  let verb = VERBS[0];
  let started = 0;
  let timer: ReturnType<typeof setInterval> | undefined;
  let current: ExtensionContext | undefined;
  // The run's output tokens: the messages it has finished, and an estimate of the one streaming.
  let finished = 0;
  let streaming = 0;

  const message = () => {
    const seconds = Math.floor((Date.now() - started) / 1000);
    const elapsed = seconds < 60 ? `${seconds}s` : `${Math.floor(seconds / 60)}m ${seconds % 60}s`;
    const key = keyText("app.interrupt").replace(/\bescape\b/, "esc");
    const tokens = finished + streaming;
    const produced = tokens ? ` · ↓ ${formatTokens(tokens)} tokens` : "";
    current?.ui.setWorkingMessage(`${label ?? verb}… (${elapsed}${produced} · ${key} to interrupt)`);
  };
  const stop = () => {
    if (timer) clearInterval(timer);
    timer = undefined;
  };

  // A throw while loading would stop pi from starting, majordomo's desk included.
  try {
    try {
      backslashNewline = editorConfigOf(JSON.parse(readFileSync(new URL("../config.json", import.meta.url), "utf8"))).config.backslashNewline;
    } catch {
      // No config: the defaults. extensions/editor-keys.ts reports a bad one.
    }

    pi.events.on("editor-submit", (request) => {
      const r = request as { text?: unknown; handled?: boolean };
      if (typeof r.text !== "string" || !activeEditor?.onSubmit) return;
      // pi's own submit path: commands, steering a running turn, history.
      activeEditor.setText("");
      activeEditor.onSubmit(r.text);
      r.handled = true;
    });

    pi.events.on("working-label", (text) => {
      label = typeof text === "string" && text ? text : undefined;
      if (timer) message();
    });

    pi.on("session_start", (_event, ctx) => {
      if (ctx.mode !== "tui") return;
      current = ctx;
      try {
        install(pi, ctx);
      } catch (error) {
        ctx.ui.notify(`pi-personal: chrome is off: ${String(error)}`, "warning");
      }
    });

    pi.events.on("attention", () => {
      if (current) bell();
    });

    pi.on("agent_start", (_event, ctx) => {
      if (ctx.mode !== "tui") return;
      current = ctx;
      verb = VERBS[Math.floor(Math.random() * VERBS.length)];
      started = Date.now();
      finished = 0;
      streaming = 0;
      stop();
      message();
      timer = setInterval(message, 1000);
      ctx.ui.setTitle(`✳ ${basename(ctx.cwd)} · working`);
    });

    pi.on("session_info_changed", (_event, ctx) => {
      if (ctx.mode === "tui") idleTitle(ctx);
    });

    pi.on("message_update", (event) => {
      const m = event.message as { role?: string; content?: unknown };
      if (m.role === "assistant") streaming = streamedTokens(m.content);
    });

    pi.on("message_end", (event) => {
      const m = event.message as { role?: string; content?: unknown; usage?: { output?: number } };
      if (m.role !== "assistant") return;
      finished += m.usage?.output || streamedTokens(m.content);
      streaming = 0;
    });

    pi.on("agent_end", (_event, ctx) => {
      stop();
      label = undefined;
      if (ctx.mode !== "tui") return;
      ctx.ui.setWorkingMessage();
      ctx.ui.setTitle(`✻ ${basename(ctx.cwd)}`);
      if (started && Date.now() - started >= LONG_RUN_MS) bell();
    });

    pi.on("session_shutdown", () => stop());
  } catch (error) {
    console.error(`pi-personal: chrome is off: ${String(error)}`);
  }
}
