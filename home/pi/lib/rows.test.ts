import assert from "node:assert/strict";
import { test } from "node:test";

import { callArgs, callTitle, collapsed, diffStats, linesOf, mcpName, oneLine, resultText, shortPath, withoutNotes } from "./rows.ts";

const cwd = "/home/me/src/app";
const home = "/home/me";

test("titles use Claude Code's names, and MCP tools read as server - tool", () => {
  assert.equal(callTitle("edit"), "Update");
  assert.equal(callTitle("grep"), "Search");
  assert.equal(callTitle("mcp__slack__search_messages"), "slack - search_messages (MCP)");
  assert.equal(callTitle("todo"), "todo");
  assert.deepEqual(mcpName("mcp__ms365__list_events"), { server: "ms365", tool: "list_events" });
  assert.equal(mcpName("read"), undefined);
});

test("paths are relative to the working directory, else under ~", () => {
  assert.equal(shortPath("/home/me/src/app/a/b.ts", cwd, home), "a/b.ts");
  assert.equal(shortPath("/home/me/notes.md", cwd, home), "~/notes.md");
  assert.equal(shortPath("/etc/hosts", cwd, home), "/etc/hosts");
});

test("call arguments show the main argument for built-ins", () => {
  assert.equal(callArgs("read", { path: "/home/me/src/app/x.ts" }, cwd, home), "x.ts");
  assert.equal(callArgs("read", { path: "x.ts", offset: 10, limit: 5 }, cwd, home), "x.ts · lines 10–14");
  assert.equal(callArgs("bash", { command: "npm\n  test" }, cwd, home), "npm test");
  assert.equal(callArgs("grep", { pattern: "TODO", path: "src" }, cwd, home), 'pattern: "TODO", path: "src"');
  assert.equal(callArgs("ls", {}, cwd, home), ".");
});

test("other tools show their first three arguments, cut short", () => {
  assert.equal(callArgs("mcp__slack__search", { query: "standup", limit: 5 }, cwd, home), 'query: "standup", limit: 5');
  assert.equal(callArgs("x", { a: 1, b: 2, c: 3, d: 4 }, cwd, home), "a: 1, b: 2, c: 3, …");
  assert.equal(callArgs("x", { text: "y".repeat(100) }, cwd, home).length < 70, true);
});

test("text helpers", () => {
  assert.equal(oneLine("a\n b   c", 10), "a b c");
  assert.equal(oneLine("abcdefghij", 5), "abcd…");
  assert.equal(resultText([{ type: "text", text: "a" }, { type: "image" }, { type: "text", text: "b" }]), "a\nb");
  assert.equal(withoutNotes("one\ntwo\n\n[Showing lines 1-2 of 9. Use offset=3 to continue.]"), "one\ntwo");
  assert.deepEqual(linesOf("a\nb\n\n"), ["a", "b"]);
  assert.deepEqual(linesOf(""), []);
});

test("diff stats count pi's numbered + and - lines", () => {
  assert.deepEqual(diffStats(" 1 one\n-2 two\n+2 2\n+3 2b\n 3 three"), { added: 2, removed: 1 });
});

test("collapsed results read like Claude Code's", () => {
  assert.deepEqual(collapsed("read", { path: "a" }, "1\n2\n3\n\n[12 more lines in file. Use offset=4 to continue.]", undefined, cwd, home), {
    head: "Read 3 lines",
    preview: "none",
    lines: 0,
  });
  assert.equal(collapsed("edit", { path: `${cwd}/a.ts` }, "", { diff: "-1 a\n+1 b" }, cwd, home).head, "Updated a.ts with 1 addition and 1 removal");
  assert.equal(collapsed("write", { path: "a.ts", content: "x\ny\n" }, "", undefined, cwd, home).head, "Wrote 2 lines to a.ts");
  assert.equal(collapsed("grep", {}, "a.ts:1: x\na.ts-2- y\nb.ts:9: z", undefined, cwd, home).head, "Found 2 lines");
  assert.equal(collapsed("grep", {}, "No matches found", undefined, cwd, home).head, "No matches");
  assert.deepEqual(collapsed("bash", {}, "", undefined, cwd, home), { head: "(No content)", preview: "none", lines: 0 });
  assert.deepEqual(collapsed("bash", {}, "out", undefined, cwd, home), { preview: "text", lines: 3 });
  assert.deepEqual(collapsed("mcp__x__y", {}, "{}", undefined, cwd, home), { preview: "text", lines: 3 });
});
