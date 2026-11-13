// Claude Code's ctrl+s, with a stack behind it (lib/drafts.ts). ctrl+s sets the editor's draft
// aside and clears the editor; on an empty editor it brings back the newest draft. After you send a
// prompt, a draft you stashed earlier in the same pi process comes back on its own, as Claude
// Code's does. alt+s (or /drafts) opens the stack, to scroll back through older drafts, restore one,
// or delete one.
//
// Drafts are kept per working directory in the agent dir (pi-personal/drafts.json, readable only by
// you), across restarts and pi windows. The keys, the limit, and the restore after sending come
// from the package's config.json, which the home-manager module writes (programs.pi-personal.drafts).
// They're extension shortcuts, which pi checks before the editor's own keys; the module moves pi's
// ctrl+s (app.thinking.save) out of the way.

import { randomUUID } from "node:crypto";
import { mkdirSync, readFileSync, renameSync, writeFileSync } from "node:fs";
import { dirname, join } from "node:path";

import { type ExtensionAPI, type ExtensionContext, getAgentDir, type KeybindingsManager, type Theme } from "@earendil-works/pi-coding-agent";
import { type Component, type KeyId, matchesKey, truncateToWidth, visibleWidth, wrapTextWithAnsi } from "@earendil-works/pi-tui";

import { age, configOf, type Draft, draftsIn, type DraftsConfig, firstLine, push, type Store, storeOf, take } from "../lib/drafts.ts";

function readJson(path: string | URL): unknown {
  try {
    return JSON.parse(readFileSync(path, "utf8"));
  } catch {
    return undefined;
  }
}

const storeFile = () => join(getAgentDir(), "pi-personal", "drafts.json");
const load = (): Store => storeOf(readJson(storeFile()));

/** Writes the store whole, through a temporary file, so a crash leaves the old one or the new. */
function save(store: Store): void {
  const file = storeFile();
  mkdirSync(dirname(file), { recursive: true, mode: 0o700 });
  const tmp = `${file}.${process.pid}.tmp`;
  writeFileSync(tmp, `${JSON.stringify(store, null, 2)}\n`, { mode: 0o600 });
  renameSync(tmp, file);
}

const BROWSER_ROWS = 8;
const PREVIEW_ROWS = 8;

/**
 * The stack in an overlay: the drafts newest first, the selected one's text under them, and the
 * keys. Enter restores the selected draft, d or delete removes it, escape closes.
 */
class DraftBrowser implements Component {
  private drafts: Draft[];
  private selected = 0;
  private readonly theme: Theme;
  private readonly keybindings: KeybindingsManager;
  private readonly title: string;
  private readonly onRestore: (draft: Draft) => void;
  private readonly onDelete: (draft: Draft) => void;
  private readonly onClose: () => void;
  private cached: { width: number; lines: string[] } | undefined;

  constructor(
    drafts: Draft[],
    title: string,
    theme: Theme,
    keybindings: KeybindingsManager,
    actions: { restore: (draft: Draft) => void; delete: (draft: Draft) => void; close: () => void },
  ) {
    this.drafts = drafts;
    this.title = title;
    this.theme = theme;
    this.keybindings = keybindings;
    this.onRestore = actions.restore;
    this.onDelete = actions.delete;
    this.onClose = actions.close;
  }

  handleInput(data: string): void {
    const k = this.keybindings;
    const last = this.drafts.length - 1;
    if (k.matches(data, "tui.select.cancel")) return this.onClose();
    if (k.matches(data, "tui.select.confirm")) {
      const draft = this.drafts[this.selected];
      return draft ? this.onRestore(draft) : this.onClose();
    }
    if (k.matches(data, "tui.select.up")) this.selected = this.selected > 0 ? this.selected - 1 : last;
    else if (k.matches(data, "tui.select.down")) this.selected = this.selected < last ? this.selected + 1 : 0;
    else if (k.matches(data, "tui.select.pageUp")) this.selected = Math.max(0, this.selected - BROWSER_ROWS);
    else if (k.matches(data, "tui.select.pageDown")) this.selected = Math.min(last, this.selected + BROWSER_ROWS);
    else if (data === "d" || matchesKey(data, "delete")) {
      const draft = this.drafts[this.selected];
      if (!draft) return;
      this.onDelete(draft);
      this.drafts = this.drafts.filter((d) => d.id !== draft.id);
      if (this.drafts.length === 0) return this.onClose();
      this.selected = Math.min(this.selected, this.drafts.length - 1);
    } else return;
    this.invalidate();
  }

  render(width: number): string[] {
    if (this.cached?.width === width) return this.cached.lines;
    const t = this.theme;
    const inner = Math.max(10, width - 4);
    const row = (text = "") => `${t.fg("borderAccent", "│")} ${truncateToWidth(text, inner, "…")}${" ".repeat(Math.max(0, inner - visibleWidth(text)))} ${t.fg("borderAccent", "│")}`;
    const rule = () => t.fg("borderAccent", `├${"─".repeat(inner + 2)}┤`);
    const head = ` ${this.title} · ${this.drafts.length} `;
    const lines = [t.fg("borderAccent", `╭─${t.bold(head)}${"─".repeat(Math.max(0, inner + 1 - visibleWidth(head)))}╮`)];

    // The list scrolls to keep the selection in view.
    const start = Math.min(Math.max(0, this.selected - BROWSER_ROWS + 1), Math.max(0, this.drafts.length - BROWSER_ROWS));
    const now = new Date();
    for (const [i, draft] of this.drafts.slice(start, start + BROWSER_ROWS).entries()) {
      const at = start + i;
      const when = age(draft.at, now).padStart(4);
      const text = firstLine(draft.text, inner - 8);
      lines.push(row(at === this.selected ? `${t.fg("accent", "▸")} ${t.fg("muted", when)}  ${t.bold(text)}` : `  ${t.fg("muted", when)}  ${text}`));
    }
    if (this.drafts.length > BROWSER_ROWS) lines.push(row(t.fg("muted", `  ${this.selected + 1} of ${this.drafts.length}`)));

    lines.push(rule());
    const preview = wrapTextWithAnsi(this.drafts[this.selected]?.text.replace(/\s+$/, "") ?? "", inner);
    for (const line of preview.slice(0, PREVIEW_ROWS)) lines.push(row(line));
    if (preview.length > PREVIEW_ROWS) lines.push(row(t.fg("muted", `… +${preview.length - PREVIEW_ROWS} lines`)));
    lines.push(rule());
    lines.push(row(t.fg("muted", "↑↓ choose · enter restore · d delete · esc close")));
    lines.push(t.fg("borderAccent", `╰${"─".repeat(inner + 2)}╯`));
    this.cached = { width, lines };
    return lines;
  }

  invalidate(): void {
    this.cached = undefined;
  }
}

export default function drafts(pi: ExtensionAPI): void {
  let config: DraftsConfig;
  let problems: string[];
  // Drafts stashed in this pi process and not yet back in the editor: the one on top comes back
  // after the next prompt is sent.
  const pending = new Set<string>();

  const showCount = (ctx: ExtensionContext) => {
    const n = draftsIn(load(), ctx.cwd).length;
    ctx.ui.setStatus("drafts", n ? ctx.ui.theme.fg("muted", `${n} stashed · ${config.keys.browse} to browse`) : undefined);
  };

  /** Puts a draft back in the editor and off the stack; text already in the editor goes on the stack first. */
  const restore = (ctx: ExtensionContext, id: string): void => {
    let store = load();
    const current = ctx.ui.getEditorText();
    const { store: rest, draft } = take(store, id);
    if (!draft) return;
    store = current.trim() ? push(rest, { id: randomUUID(), text: current, cwd: ctx.cwd, at: new Date().toISOString() }, config.limit) : rest;
    save(store);
    pending.delete(id);
    ctx.ui.setEditorText(draft.text);
    if (current.trim()) ctx.ui.notify("Draft restored; the editor's text went on the stack", "info");
  };

  const stash = (ctx: ExtensionContext): void => {
    const text = ctx.ui.getEditorText();
    if (text.trim()) {
      const draft = { id: randomUUID(), text, cwd: ctx.cwd, at: new Date().toISOString() };
      save(push(load(), draft, config.limit));
      pending.add(draft.id);
      ctx.ui.setEditorText("");
      ctx.ui.notify(`Stashed · ${config.keys.stash} to restore`, "info");
    } else {
      const top = draftsIn(load(), ctx.cwd)[0];
      if (top) restore(ctx, top.id);
      else ctx.ui.notify("Nothing stashed here", "info");
    }
    showCount(ctx);
  };

  const browse = async (ctx: ExtensionContext): Promise<void> => {
    const list = draftsIn(load(), ctx.cwd);
    if (list.length === 0) {
      ctx.ui.notify("Nothing stashed here", "info");
      return;
    }
    const chosen = await ctx.ui.custom<Draft | undefined>(
      (_tui, theme, keybindings, done) =>
        new DraftBrowser(list, "Drafts", theme, keybindings, {
          restore: (draft) => done(draft),
          delete: (draft) => {
            save(take(load(), draft.id).store);
            pending.delete(draft.id);
          },
          close: () => done(undefined),
        }),
      { overlay: true, overlayOptions: { width: "80%", minWidth: 40, maxHeight: "80%", anchor: "center" } },
    );
    if (chosen) restore(ctx, chosen.id);
    showCount(ctx);
  };

  // A throw while loading would stop pi from starting, majordomo's desk included.
  try {
    ({ config, problems } = configOf(readJson(new URL("../config.json", import.meta.url))));
    const guarded = (action: (ctx: ExtensionContext) => void | Promise<void>) => async (ctx: ExtensionContext) => {
      try {
        await action(ctx);
      } catch (error) {
        ctx.ui.notify(`Drafts: ${String(error)}`, "error");
      }
    };
    pi.registerShortcut(config.keys.stash as KeyId, { description: "Stash the draft, or restore the newest stashed one", handler: guarded(stash) });
    pi.registerShortcut(config.keys.browse as KeyId, { description: "Browse stashed drafts", handler: guarded(browse) });
    pi.registerCommand("drafts", { description: "Browse stashed drafts: restore or delete one", handler: (_args, ctx) => guarded(browse)(ctx) });

    pi.on("session_start", (_event, ctx) => {
      if (ctx.mode !== "tui") return;
      for (const problem of problems) ctx.ui.notify(`pi-personal: ${problem}`, "warning");
      showCount(ctx);
    });

    pi.on("input", (event, ctx) => {
      if (!config.restoreAfterSubmit || event.source !== "interactive" || ctx.mode !== "tui") return;
      try {
        const top = draftsIn(load(), ctx.cwd)[0];
        if (!top || !pending.has(top.id) || ctx.ui.getEditorText().trim()) return;
        restore(ctx, top.id);
        showCount(ctx);
        ctx.ui.notify("Draft restored", "info");
      } catch {
        // The prompt goes ahead either way.
      }
    });
  } catch (error) {
    console.error(`pi-personal: drafts are off: ${String(error)}`);
  }
}
