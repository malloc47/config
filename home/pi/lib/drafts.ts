// Stashed drafts: unsent editor text set aside on a stack, newest first, kept per working
// directory and across restarts. Pure, so it's tested without pi; extensions/drafts.ts binds it to
// the editor, the keys, and a browser.

export type Draft = { id: string; text: string; cwd: string; at: string };
export type Store = { version: 1; drafts: Draft[] };

export type DraftsConfig = {
  keys: { stash: string; browse: string };
  /** Drafts kept per working directory; a stash past it drops the oldest. */
  limit: number;
  /** After a prompt is sent, put back a draft stashed earlier in the same pi process, as Claude Code does. */
  restoreAfterSubmit: boolean;
};

export const DEFAULTS: DraftsConfig = { keys: { stash: "ctrl+s", browse: "alt+s" }, limit: 50, restoreAfterSubmit: true };

const MODIFIERS = new Set(["ctrl", "shift", "alt", "super"]);
const NAMED = new Set(["escape", "esc", "enter", "return", "tab", "space", "backspace", "delete", "insert", "clear", "home", "end", "pageup", "pagedown", "up", "down", "left", "right"]);

/** Whether a key reads as pi's key syntax: modifiers joined by "+", then one key ("ctrl+s", "alt+shift+f2"). */
export function validKey(key: string): boolean {
  const parts = key.toLowerCase().split("+");
  const last = parts.pop() ?? "";
  if (parts.some((p) => !MODIFIERS.has(p)) || new Set(parts).size !== parts.length) return false;
  return /^[a-z0-9]$/.test(last) || /^f([1-9]|1[0-2])$/.test(last) || NAMED.has(last) || /^[`\-=[\]\\;',./!@#$%^&*()_+|~{}:<>?]$/.test(last);
}

/** The configuration from the package's config.json, each bad value replaced by its default and named in `problems`. */
export function configOf(json: unknown): { config: DraftsConfig; problems: string[] } {
  const given = (json as { drafts?: Record<string, unknown> } | undefined)?.drafts ?? {};
  const problems: string[] = [];
  const keysGiven = (given.keys ?? {}) as Record<string, unknown>;
  const key = (name: "stash" | "browse") => {
    const value = keysGiven[name];
    if (value === undefined) return DEFAULTS.keys[name];
    if (typeof value === "string" && validKey(value)) return value.toLowerCase();
    problems.push(`drafts.keys.${name} (${JSON.stringify(value)}) isn't a key; using ${DEFAULTS.keys[name]}`);
    return DEFAULTS.keys[name];
  };
  const keys = { stash: key("stash"), browse: key("browse") };
  if (keys.stash === keys.browse) {
    problems.push(`drafts.keys.stash and browse are both ${keys.stash}; using the defaults`);
    Object.assign(keys, DEFAULTS.keys);
  }
  let limit = DEFAULTS.limit;
  if (given.limit !== undefined) {
    if (Number.isInteger(given.limit) && (given.limit as number) > 0) limit = given.limit as number;
    else problems.push(`drafts.limit (${JSON.stringify(given.limit)}) isn't a positive whole number; using ${DEFAULTS.limit}`);
  }
  const restoreAfterSubmit = typeof given.restoreAfterSubmit === "boolean" ? given.restoreAfterSubmit : DEFAULTS.restoreAfterSubmit;
  return { config: { keys, limit, restoreAfterSubmit }, problems };
}

/** A store from its file's JSON; anything unreadable counts as empty, and malformed drafts are dropped. */
export function storeOf(json: unknown): Store {
  const drafts = (json as { drafts?: unknown } | undefined)?.drafts;
  if (!Array.isArray(drafts)) return { version: 1, drafts: [] };
  const valid = drafts.filter(
    (d): d is Draft => d !== null && typeof d === "object" && typeof d.id === "string" && typeof d.text === "string" && typeof d.cwd === "string" && typeof d.at === "string",
  );
  return { version: 1, drafts: valid };
}

/** The drafts stashed in a working directory, newest first. */
export function draftsIn(store: Store, cwd: string): Draft[] {
  return store.drafts.filter((d) => d.cwd === cwd);
}

/** The store with a draft on top of its directory's stack, keeping at most `limit` there. Blank text changes nothing. */
export function push(store: Store, draft: Draft, limit: number): Store {
  if (!draft.text.trim()) return store;
  const kept = draftsIn(store, draft.cwd).slice(0, Math.max(0, limit - 1));
  const others = store.drafts.filter((d) => d.cwd !== draft.cwd);
  return { version: 1, drafts: [draft, ...kept, ...others] };
}

/** The store without one draft, and the draft. */
export function take(store: Store, id: string): { store: Store; draft: Draft | undefined } {
  const draft = store.drafts.find((d) => d.id === id);
  return { store: { version: 1, drafts: store.drafts.filter((d) => d.id !== id) }, draft };
}

/** How long ago, briefly: "now", "4m", "3h", "2d", "5w". */
export function age(at: string, now: Date): string {
  const ms = now.getTime() - new Date(at).getTime();
  if (!Number.isFinite(ms)) return "?";
  const minutes = Math.floor(ms / 60_000);
  if (minutes < 1) return "now";
  if (minutes < 60) return `${minutes}m`;
  const hours = Math.floor(minutes / 60);
  if (hours < 24) return `${hours}h`;
  const days = Math.floor(hours / 24);
  return days < 14 ? `${days}d` : `${Math.floor(days / 7)}w`;
}

/** A draft's first non-blank line, cut to `max` columns, with how many more lines it has. */
export function firstLine(text: string, max: number): string {
  const lines = text.split("\n").filter((l) => l.trim());
  const first = (lines[0] ?? "").replace(/\s+/g, " ").trim();
  const more = lines.length > 1 ? ` (+${lines.length - 1})` : "";
  const room = Math.max(1, max - more.length);
  const cut = [...first].length > room ? `${[...first].slice(0, room - 1).join("")}…` : first;
  return `${cut}${more}`;
}
