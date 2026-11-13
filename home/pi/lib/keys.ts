// pi's key syntax, for the keys the package's config.json sets: modifiers joined by "+", then one
// key ("ctrl+s", "alt+shift+f2"). Pure, so it's tested without pi.

const MODIFIERS = new Set(["ctrl", "shift", "alt", "super"]);
const NAMED = new Set(["escape", "esc", "enter", "return", "tab", "space", "backspace", "delete", "insert", "clear", "home", "end", "pageup", "pagedown", "up", "down", "left", "right"]);

export function validKey(key: string): boolean {
  const parts = key.toLowerCase().split("+");
  const last = parts.pop() ?? "";
  if (parts.some((p) => !MODIFIERS.has(p)) || new Set(parts).size !== parts.length) return false;
  return /^[a-z0-9]$/.test(last) || /^f([1-9]|1[0-2])$/.test(last) || NAMED.has(last) || /^[`\-=[\]\\;',./!@#$%^&*()_+|~{}:<>?]$/.test(last);
}
