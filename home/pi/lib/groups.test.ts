import assert from "node:assert/strict";
import { test } from "node:test";

import { blocksOf, configOf, DEFAULTS, globMatch, grouped, itemAt, itemLabel, layout, runState, runsOf } from "./groups.ts";

const call = (id: string, name: string) => ({ type: "toolCall", id, name, arguments: {} });
const assistant = (...content: unknown[]) => ({ role: "assistant", content });
const text = (t: string) => ({ type: "text", text: t });
const thinking = { type: "thinking", thinking: "Let me look." };
const result = (id: string) => ({ role: "toolResult", toolCallId: id, content: [] });
const isMcp = (name: string) => grouped(DEFAULTS, name);

test("the configuration falls back to defaults, naming each bad value", () => {
  assert.deepEqual(configOf(undefined), { config: DEFAULTS, problems: [] });
  const given = { toolGroups: { enable: false, include: ["mcp__*", "todo"], exclude: ["mcp__github__*"], labels: { ms365: "Outlook" } } };
  assert.deepEqual(configOf(given), { config: given.toolGroups, problems: [] });
  const bad = configOf({ toolGroups: { include: "mcp__*", exclude: [""], labels: { ms365: 3 } } });
  assert.deepEqual(bad.config, DEFAULTS);
  assert.equal(bad.problems.length, 3);
});

test("globs match whole tool names, with * for any run of characters", () => {
  assert.ok(globMatch("mcp__*", "mcp__slack__read_channel"));
  assert.ok(globMatch("mcp__*__get_*", "mcp__servicenow__get_reservation"));
  assert.ok(globMatch("todo", "todo"));
  assert.ok(!globMatch("todo", "todos"));
  assert.ok(!globMatch("mcp__*", "bash"));
  assert.ok(globMatch("a.b", "a.b") && !globMatch("a.b", "axb"), "other characters match themselves");
});

test("exclude wins over include, and a disabled configuration groups nothing", () => {
  const config = { ...DEFAULTS, exclude: ["mcp__github__*"] };
  assert.ok(grouped(config, "mcp__slack__read_channel"));
  assert.ok(!grouped(config, "mcp__github__get_issue"));
  assert.ok(!grouped(config, "read"), "built-ins aren't grouped by default");
  assert.ok(!grouped({ ...DEFAULTS, enable: false }, "mcp__slack__read_channel"));
});

test("a run is the grouped calls with nothing visible between them", () => {
  const messages = [
    { role: "user", content: "What's on today?" },
    assistant(thinking, call("a", "mcp__slack__read_channel"), call("b", "mcp__ms365__list_mail_messages")),
    result("a"),
    result("b"),
    assistant(thinking, call("c", "mcp__servicenow__get_reservation")),
    result("c"),
    { role: "custom", display: false, content: "hidden context" },
    assistant(call("d", "mcp__slack__read_thread")),
    result("d"),
    assistant(text("Here's your day."), call("e", "mcp__slack__read_channel")),
    assistant(call("f", "bash"), call("g", "mcp__slack__read_channel")),
  ];
  const runs = runsOf(blocksOf(messages), isMcp);
  assert.deepEqual(runs.get("a"), { anchor: "a", members: ["a", "b", "c", "d"] }, "thinking and hidden messages don't end a run");
  assert.equal(runs.get("d"), runs.get("a"));
  assert.deepEqual(runs.get("e"), { anchor: "e", members: ["e"] }, "the model's text ends a run");
  assert.equal(runs.get("f"), undefined, "an ungrouped tool isn't in a run");
  assert.deepEqual(runs.get("g"), { anchor: "g", members: ["g"] }, "and its row ends the run before it");
});

test("your messages and displayed custom messages end a run, and a call seen twice keeps its place", () => {
  const messages = [
    assistant(call("a", "mcp__x__one")),
    { role: "custom", display: true, content: "shown" },
    assistant(call("b", "mcp__x__two")),
    { role: "user", content: "and?" },
    assistant(call("c", "mcp__x__three")),
    assistant(call("c", "mcp__x__three"), call("d", "mcp__x__four")),
  ];
  const runs = runsOf(blocksOf(messages), isMcp);
  assert.deepEqual([...new Set(runs.values())], [
    { anchor: "a", members: ["a"] },
    { anchor: "b", members: ["b"] },
    { anchor: "c", members: ["c", "d"] },
  ]);
  assert.deepEqual(blocksOf([null, 3, { role: "assistant", content: "not a list" }, assistant(text("   "))]), [], "malformed and blank parts show nothing");
});

test("labels read as the server and the tool in words", () => {
  const where = { cwd: "/data", home: "/home/u" };
  assert.equal(itemLabel("mcp__slack__slack_read_channel", {}, {}, where), "Slack(read channel)", "without the server's name repeated");
  assert.equal(itemLabel("mcp__ms365__list_mail_messages", {}, { ms365: "Outlook" }, where), "Outlook(list mail messages)");
  assert.equal(itemLabel("mcp__servicenow__get-reservation", {}, { servicenow: "ServiceNow" }, where), "ServiceNow(get reservation)");
  const long = itemLabel("mcp__servicenow__get_reservation_details_for_the_trip_and_its_hotels", {}, { servicenow: "ServiceNow" }, where);
  assert.equal(long.length, 40);
  assert.ok(long.startsWith("ServiceNow(get reservation") && long.endsWith("…)"));
});

test("built-ins read as their rows do, and other tools by their names", () => {
  const where = { cwd: "/data", home: "/home/u" };
  assert.equal(itemLabel("read", { path: "/data/USER.md" }, {}, where), "Read(USER.md)");
  assert.equal(itemLabel("edit", { path: "/data/LOOPS.md" }, {}, where), "Update(LOOPS.md)");
  assert.equal(itemLabel("bash", { command: "git status --short" }, {}, where), "Bash(git status --short)");
  assert.equal(itemLabel("ls", {}, {}, where), "List(.)");
  const cut = itemLabel("bash", { command: `echo ${"x".repeat(80)}` }, {}, where);
  assert.equal(cut.length, 40);
  assert.ok(cut.startsWith("Bash(echo xxx") && cut.endsWith("…)"));
  assert.equal(itemLabel("journal", { text: "Booked the hotel" }, {}, where), "Journal");
  assert.equal(itemLabel("write", {}, {}, where), "Write", "nothing inside, no parentheses");
});

test("a run's bullet is pending while any call runs, else red if any failed", () => {
  assert.equal(runState(["done", "pending", "error"]), "pending");
  assert.equal(runState(["done", "error"]), "error");
  assert.equal(runState(["done", "done"]), "done");
});

test("items wrap between items under the prefix, and clicks find them", () => {
  const items = [
    { id: "a", label: "Slack(read channel)" },
    { id: "b", label: "Outlook(list mail messages)" },
    { id: "c", label: "ServiceNow(get reservation)" },
  ];
  // "⏺ Tools: " is 9 columns.
  const wide = layout(items, 9, 120);
  assert.deepEqual(
    wide.map((p) => [p.id, p.line, p.from, p.to, p.text]),
    [
      ["a", 0, 9, 29, "Slack(read channel),"],
      ["b", 0, 30, 58, "Outlook(list mail messages),"],
      ["c", 0, 59, 86, "ServiceNow(get reservation)"],
    ],
  );
  const narrow = layout(items, 9, 60);
  assert.deepEqual(narrow.map((p) => [p.id, p.line, p.from]), [["a", 0, 9], ["b", 0, 30], ["c", 1, 9]]);
  assert.equal(itemAt(narrow, 9, 0, 3), "all", "the prefix stands for every item");
  assert.equal(itemAt(narrow, 9, 0, 35), "b");
  assert.equal(itemAt(narrow, 9, 1, 12), "c");
  assert.equal(itemAt(narrow, 9, 1, 50), undefined, "past the last item");
  const tiny = layout([{ id: "a", label: "Outlook(list mail messages)" }], 9, 20);
  assert.equal(tiny[0].to - tiny[0].from, 11, "a label too wide for a line is cut to fit");
});
