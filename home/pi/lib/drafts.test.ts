import assert from "node:assert/strict";
import { test } from "node:test";

import { age, configOf, DEFAULTS, type Draft, draftsIn, firstLine, push, storeOf, take, validKey } from "./drafts.ts";

const draft = (id: string, cwd = "/a", text = `text ${id}`): Draft => ({ id, text, cwd, at: "2026-11-13T10:00:00Z" });

test("keys read as pi's key syntax", () => {
  for (const key of ["ctrl+s", "alt+s", "ctrl+alt+s", "shift+f2", "ctrl+/", "alt+pageDown"]) assert.ok(validKey(key), key);
  for (const key of ["", "s+ctrl", "ctrl+ctrl+s", "hyper+s", "ctrl+", "ctrl+ss"]) assert.ok(!validKey(key), key);
});

test("the configuration falls back to defaults, naming each bad value", () => {
  assert.deepEqual(configOf(undefined), { config: DEFAULTS, problems: [] });
  assert.deepEqual(configOf({ drafts: { keys: { browse: "Ctrl+Alt+D" }, limit: 9, restoreAfterSubmit: false } }).config, {
    keys: { stash: "ctrl+s", browse: "ctrl+alt+d" },
    limit: 9,
    restoreAfterSubmit: false,
  });
  const bad = configOf({ drafts: { keys: { stash: "nope+s" }, limit: 0 } });
  assert.deepEqual(bad.config, DEFAULTS);
  assert.equal(bad.problems.length, 2);
  assert.deepEqual(configOf({ drafts: { keys: { stash: "alt+s" } } }).config.keys, DEFAULTS.keys, "stash and browse on one key");
});

test("an unreadable store is empty, and malformed drafts drop out", () => {
  assert.deepEqual(storeOf(undefined), { version: 1, drafts: [] });
  assert.deepEqual(storeOf({ drafts: [draft("1"), { id: 2 }, null] }).drafts, [draft("1")]);
});

test("a stash goes on top of its directory's stack, which keeps at most the limit", () => {
  let store = storeOf({});
  for (const id of ["1", "2", "3"]) store = push(store, draft(id), 2);
  store = push(store, draft("b", "/b"), 2);
  assert.deepEqual(draftsIn(store, "/a").map((d) => d.id), ["3", "2"]);
  assert.deepEqual(draftsIn(store, "/b").map((d) => d.id), ["b"]);
  assert.equal(push(store, draft("x", "/a", "  \n "), 2), store, "a blank draft changes nothing");
});

test("taking a draft removes it", () => {
  const store = push(push(storeOf({}), draft("1"), 5), draft("2"), 5);
  const { store: after, draft: taken } = take(store, "1");
  assert.equal(taken?.id, "1");
  assert.deepEqual(after.drafts.map((d) => d.id), ["2"]);
  assert.equal(take(store, "missing").draft, undefined);
});

test("ages and first lines read briefly", () => {
  const now = new Date("2026-11-13T12:00:00Z");
  assert.equal(age("2026-11-13T11:59:40Z", now), "now");
  assert.equal(age("2026-11-13T11:56:00Z", now), "4m");
  assert.equal(age("2026-11-13T09:00:00Z", now), "3h");
  assert.equal(age("2026-11-10T12:00:00Z", now), "3d");
  assert.equal(age("2026-10-01T12:00:00Z", now), "6w");
  assert.equal(firstLine("\n  Fix   the footer\nand the gauge\n", 40), "Fix the footer (+1)");
  assert.equal(firstLine("x".repeat(30), 10), "xxxxxxxxx…");
});
