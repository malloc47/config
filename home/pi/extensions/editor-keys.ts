// Claude Code's prompt keys (lib/editor.ts): ctrl+l clears the prompt, and ctrl+r searches the
// prompts you've sent, in this folder's sessions or, with ctrl+s, every folder's. As in Claude
// Code, in the search ctrl+r and ↑ go to older matches and ↓ to newer, Enter sends the prompt,
// Tab or Esc puts it in the editor to change first, and ctrl+c cancels. A draft the editor holds
// goes on the drafts stack before either replaces it (extensions/drafts.ts), and comes back after.
//
// The keys come from the package's config.json (programs.pi-personal.editor); the module moves
// pi's own ctrl+l (the model picker, now alt+p) and ctrl+r (renaming in the session picker) aside.
// Sending goes through the prompt editor in extensions/chrome.ts, so it's pi's own submit path.

import { readFileSync } from "node:fs";

import { type ExtensionAPI, type ExtensionContext, type KeybindingsManager, SessionManager, type Theme } from "@earendil-works/pi-coding-agent";
import { type Component, type KeyId, matchesKey, type TUI, truncateToWidth, visibleWidth, wrapTextWithAnsi } from "@earendil-works/pi-tui";

import { configOf, type EditorConfig, matching, newestFirst, type Prompt, promptsOf } from "../lib/editor.ts";
import { age, firstLine } from "../lib/drafts.ts";
import { shortCwd } from "../lib/chrome.ts";

type Scope = "folder" | "all";
type Choice = { action: "run" | "edit"; text: string } | undefined;

/** Sessions read per search, newest first: enough history without opening every file. */
const SESSIONS_READ = 300;
const LIST_ROWS = 8;
const PREVIEW_ROWS = 6;

/** The prompts sent in this folder's sessions, or every folder's, newest first, this session's included. */
async function loadPrompts(ctx: ExtensionContext, scope: Scope, limit: number): Promise<Prompt[]> {
  const sessions = scope === "folder" ? await SessionManager.list(ctx.cwd, ctx.sessionManager.getSessionDir()) : await SessionManager.listAll();
  const prompts: Prompt[] = promptsOf(ctx.sessionManager.getEntries() as never, ctx.cwd);
  const current = ctx.sessionManager.getSessionFile();
  for (const info of [...sessions].sort((a, b) => b.modified.getTime() - a.modified.getTime()).slice(0, SESSIONS_READ)) {
    if (info.path === current) continue;
    try {
      prompts.push(...promptsOf(SessionManager.open(info.path).getEntries() as never, info.cwd));
    } catch {
      // A session pi can't read has no prompts to offer.
    }
  }
  return newestFirst(prompts, limit);
}

/** The search: a query line, the matching prompts, the selected one in full, and the keys. */
class HistorySearch implements Component {
  private query = "";
  private scope: Scope = "folder";
  private prompts: Prompt[] | undefined;
  private matches: Prompt[] = [];
  private selected = 0;
  private cached: { width: number; lines: string[] } | undefined;
  private readonly tui: TUI;
  private readonly theme: Theme;
  private readonly keybindings: KeybindingsManager;
  private readonly load: (scope: Scope) => Promise<Prompt[]>;
  private readonly done: (choice: Choice) => void;
  private readonly home: string | undefined = process.env.HOME;

  constructor(tui: TUI, theme: Theme, keybindings: KeybindingsManager, load: (scope: Scope) => Promise<Prompt[]>, done: (choice: Choice) => void) {
    this.tui = tui;
    this.theme = theme;
    this.keybindings = keybindings;
    this.load = load;
    this.done = done;
    this.reload();
  }

  private reload(): void {
    this.prompts = undefined;
    const scope = this.scope;
    this.load(scope).then(
      (prompts) => {
        if (scope !== this.scope) return;
        this.prompts = prompts;
        this.refilter();
      },
      () => {
        this.prompts = [];
        this.refilter();
      },
    );
  }

  private refilter(): void {
    this.matches = matching(this.prompts ?? [], this.query);
    this.selected = Math.min(this.selected, Math.max(0, this.matches.length - 1));
    this.invalidate();
    this.tui.requestRender();
  }

  handleInput(data: string): void {
    const chosen = this.matches[this.selected];
    if (matchesKey(data, "ctrl+c")) return this.done(undefined);
    if (this.keybindings.matches(data, "tui.input.submit")) return this.done(chosen && { action: "run", text: chosen.text });
    if (matchesKey(data, "escape") || matchesKey(data, "tab")) return this.done(chosen && { action: "edit", text: chosen.text });
    const last = this.matches.length - 1;
    if (matchesKey(data, "ctrl+r") || this.keybindings.matches(data, "tui.select.up")) this.selected = Math.min(last, this.selected + 1);
    else if (this.keybindings.matches(data, "tui.select.down")) this.selected = Math.max(0, this.selected - 1);
    else if (this.keybindings.matches(data, "tui.select.pageUp")) this.selected = Math.min(last, this.selected + LIST_ROWS);
    else if (this.keybindings.matches(data, "tui.select.pageDown")) this.selected = Math.max(0, this.selected - LIST_ROWS);
    else if (matchesKey(data, "ctrl+s")) {
      this.scope = this.scope === "folder" ? "all" : "folder";
      this.selected = 0;
      this.reload();
    } else if (matchesKey(data, "backspace")) {
      this.query = [...this.query].slice(0, -1).join("");
      this.selected = 0;
      return this.refilter();
    } else if (matchesKey(data, "ctrl+u")) {
      this.query = "";
      this.selected = 0;
      return this.refilter();
    } else {
      // Typed or pasted text extends the query.
      const text = data.replace(/\x1b\[20[01]~/g, "");
      if (!text || /[\x00-\x1f\x7f]/.test(text)) return;
      this.query += text;
      this.selected = 0;
      return this.refilter();
    }
    this.invalidate();
  }

  render(width: number): string[] {
    if (this.cached?.width === width) return this.cached.lines;
    const t = this.theme;
    const inner = Math.max(10, width - 4);
    const row = (text = "") => `${t.fg("borderAccent", "│")} ${truncateToWidth(text, inner, "…")}${" ".repeat(Math.max(0, inner - visibleWidth(text)))} ${t.fg("borderAccent", "│")}`;
    const rule = () => t.fg("borderAccent", `├${"─".repeat(inner + 2)}┤`);
    const where = this.scope === "folder" ? "this folder" : "all folders";
    const head = ` Search prompts · ${where} `;
    const lines = [t.fg("borderAccent", `╭─${t.bold(head)}${"─".repeat(Math.max(0, inner + 1 - visibleWidth(head)))}╮`)];
    lines.push(row(`${t.fg("accent", "❯")} ${this.query}${t.inverse(" ")}`));
    lines.push(rule());

    if (this.prompts === undefined) lines.push(row(t.fg("muted", "Reading sessions…")));
    else if (this.matches.length === 0) lines.push(row(t.fg("muted", this.prompts.length ? "No match" : "No prompts yet")));
    else {
      // The list scrolls to keep the selection in view; the newest match is at the top.
      const start = Math.min(Math.max(0, this.selected - LIST_ROWS + 1), Math.max(0, this.matches.length - LIST_ROWS));
      const now = new Date();
      for (const [i, prompt] of this.matches.slice(start, start + LIST_ROWS).entries()) {
        const when = age(prompt.at, now).padStart(4);
        const dir = this.scope === "all" ? t.fg("muted", ` · ${shortCwd(prompt.cwd, this.home)}`) : "";
        const text = firstLine(prompt.text, inner - 8 - visibleWidth(dir));
        lines.push(row(start + i === this.selected ? `${t.fg("accent", "▸")} ${t.fg("muted", when)}  ${t.bold(text)}${dir}` : `  ${t.fg("muted", when)}  ${text}${dir}`));
      }
      if (this.matches.length > LIST_ROWS) lines.push(row(t.fg("muted", `  ${this.selected + 1} of ${this.matches.length}`)));
      lines.push(rule());
      const preview = wrapTextWithAnsi(this.matches[this.selected]?.text ?? "", inner);
      for (const line of preview.slice(0, PREVIEW_ROWS)) lines.push(row(line));
      if (preview.length > PREVIEW_ROWS) lines.push(row(t.fg("muted", `… +${preview.length - PREVIEW_ROWS} lines`)));
    }
    lines.push(rule());
    const other = this.scope === "folder" ? "all folders" : "this folder";
    lines.push(row(t.fg("muted", `ctrl+r older · enter send · tab edit · ctrl+s ${other} · ctrl+c cancel`)));
    lines.push(t.fg("borderAccent", `╰${"─".repeat(inner + 2)}╯`));
    this.cached = { width, lines };
    return lines;
  }

  invalidate(): void {
    this.cached = undefined;
  }
}

export default function editorKeys(pi: ExtensionAPI): void {
  let config: EditorConfig;
  let problems: string[];

  /** Moves the editor's text, if any, onto the drafts stack; false if it couldn't go there. */
  const stashDraft = (ctx: ExtensionContext): boolean => {
    const text = ctx.ui.getEditorText();
    if (!text.trim()) return true;
    const request = { text, ctx, handled: false };
    pi.events.emit("drafts:stash", request);
    if (request.handled) ctx.ui.notify("Your draft went on the stash; it comes back after this prompt", "info");
    return request.handled;
  };

  const search = async (ctx: ExtensionContext): Promise<void> => {
    const choice = await ctx.ui.custom<Choice>(
      (tui, theme, keybindings, done) => new HistorySearch(tui, theme, keybindings, (scope) => loadPrompts(ctx, scope, config.historyLimit), done),
      { overlay: true, overlayOptions: { width: "80%", minWidth: 40, maxHeight: "80%", anchor: "center" } },
    );
    if (!choice) return;
    // A draft that can't go on the stash stays: the chosen prompt doesn't replace it.
    if (!stashDraft(ctx)) {
      ctx.ui.notify("The editor has a draft; clear or stash it first", "warning");
      return;
    }
    if (choice.action === "edit") {
      ctx.ui.setEditorText(choice.text);
      return;
    }
    const request = { text: choice.text, handled: false };
    pi.events.emit("editor-submit", request);
    if (!request.handled) {
      ctx.ui.setEditorText(choice.text);
      ctx.ui.notify("Press enter to send it", "info");
    }
  };

  // A throw while loading would stop pi from starting, majordomo's desk included.
  try {
    let json: unknown;
    try {
      json = JSON.parse(readFileSync(new URL("../config.json", import.meta.url), "utf8"));
    } catch {
      json = undefined;
    }
    ({ config, problems } = configOf(json));
    const guarded = (action: (ctx: ExtensionContext) => void | Promise<void>) => async (ctx: ExtensionContext) => {
      try {
        await action(ctx);
      } catch (error) {
        ctx.ui.notify(`pi-personal: ${String(error)}`, "error");
      }
    };
    pi.registerShortcut(config.keys.clear as KeyId, { description: "Clear the prompt", handler: guarded((ctx) => ctx.ui.setEditorText("")) });
    pi.registerShortcut(config.keys.history as KeyId, { description: "Search the prompts you've sent", handler: guarded(search) });
    pi.registerCommand("history", { description: "Search the prompts you've sent", handler: (_args, ctx) => guarded(search)(ctx) });
    pi.on("session_start", (_event, ctx) => {
      if (ctx.mode !== "tui") return;
      for (const problem of problems) ctx.ui.notify(`pi-personal: ${problem}`, "warning");
    });
  } catch (error) {
    console.error(`pi-personal: prompt keys are off: ${String(error)}`);
  }
}
