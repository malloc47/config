import assert from "node:assert/strict";
import { test } from "node:test";

import { backslashBeforeCursor, configOf, DEFAULTS, matching, newestFirst, promptsOf } from "./editor.ts";

test("the configuration falls back to defaults, naming each bad value", () => {
  assert.deepEqual(configOf(undefined), { config: DEFAULTS, problems: [] });
  assert.deepEqual(configOf({ editor: { keys: { history: "Alt+R" }, backslashNewline: false, historyLimit: 20 } }).config, {
    keys: { clear: "ctrl+l", history: "alt+r" },
    backslashNewline: false,
    historyLimit: 20,
  });
  const bad = configOf({ editor: { keys: { clear: "meta+l" }, historyLimit: -1 } });
  assert.deepEqual(bad.config, DEFAULTS);
  assert.equal(bad.problems.length, 2);
  assert.deepEqual(configOf({ editor: { keys: { clear: "ctrl+r" } } }).config.keys, DEFAULTS.keys, "clear and history on one key");
});

test("a backslash counts only right before the cursor", () => {
  assert.ok(backslashBeforeCursor(["first \\"], { line: 0, col: 7 }));
  assert.ok(backslashBeforeCursor(["a", "b\\c"], { line: 1, col: 2 }));
  assert.ok(!backslashBeforeCursor(["first \\ x"], { line: 0, col: 9 }));
  assert.ok(!backslashBeforeCursor([""], { line: 0, col: 0 }));
});

test("prompts come from user messages, without other programs' envelopes", () => {
  const entries = [
    { type: "message", timestamp: "2026-11-13T10:00:00Z", message: { role: "user", content: "Fix the footer" } },
    { type: "message", timestamp: "2026-11-13T10:01:00Z", message: { role: "assistant", content: "ok" } },
    { type: "message", timestamp: "2026-11-13T10:02:00Z", message: { role: "user", content: [{ type: "text", text: "Ask " }, { type: "image" }, { type: "text", text: "Carol" }] } },
    { type: "message", timestamp: "2026-11-13T10:03:00Z", message: { role: "user", content: '<routine name="heartbeat" at="…"></routine>' } },
    { type: "message", timestamp: "2026-11-13T10:04:00Z", message: { role: "user", content: "<inbound surface=\"cli\">hi</inbound>" } },
    { type: "message", timestamp: "2026-11-13T10:05:00Z", message: { role: "user", content: "<b> is bold, right?" } },
    { type: "custom", timestamp: "2026-11-13T10:06:00Z" },
  ];
  assert.deepEqual(
    promptsOf(entries, "/a").map((p) => p.text),
    ["Fix the footer", "Ask Carol", "<b> is bold, right?"],
  );
});

test("history is newest first, once per text, and searched by every word", () => {
  const p = (text: string, at: string) => ({ text, at, cwd: "/a" });
  const history = newestFirst([p("fix the footer", "1"), p("ask carol", "2"), p("fix the footer", "3"), p("footer tokens", "4")], 10);
  assert.deepEqual(history.map((h) => h.text), ["footer tokens", "fix the footer", "ask carol"]);
  assert.equal(newestFirst(history, 1).length, 1);
  assert.deepEqual(matching(history, "FOOTER fix").map((h) => h.text), ["fix the footer"]);
  assert.equal(matching(history, "  ").length, 3);
});
