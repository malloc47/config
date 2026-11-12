import assert from "node:assert/strict";
import { test } from "node:test";

import { contextGauge, formatTokens, shortCwd, streamedTokens, usageTotals } from "./chrome.ts";

test("usage totals add every entry that carries usage", () => {
  const entries = [
    { type: "message", message: { role: "assistant", usage: { input: 100, output: 20, cost: { total: 0.5 } } } },
    { type: "message", message: { role: "user" } },
    { type: "message", message: { role: "toolResult", usage: { input: 5, output: 1, cost: { total: 0.01 } } } },
    { type: "compaction", usage: { input: 1000, output: 50, cost: { total: 0.2 } } },
    { type: "custom" },
  ];
  const totals = usageTotals(entries);
  assert.equal(totals.input, 1105);
  assert.equal(totals.output, 71);
  assert.ok(Math.abs(totals.cost - 0.71) < 1e-9);
});

test("tokens read compactly", () => {
  assert.equal(formatTokens(950), "950");
  assert.equal(formatTokens(9500), "9.5k");
  assert.equal(formatTokens(95_400), "95k");
  assert.equal(formatTokens(1_250_000), "1.3M");
});

test("the working directory reads under ~", () => {
  assert.equal(shortCwd("/home/me", "/home/me"), "~");
  assert.equal(shortCwd("/home/me/src", "/home/me"), "~/src");
  assert.equal(shortCwd("/home/meg", "/home/me"), "/home/meg");
  assert.equal(shortCwd("/tmp", undefined), "/tmp");
});

test("the context gauge fills eight cells and turns at 70% and 90%", () => {
  assert.deepEqual(contextGauge(0), { bar: "▱▱▱▱▱▱▱▱", label: "0%", tone: "ok" });
  assert.deepEqual(contextGauge(50), { bar: "▰▰▰▰▱▱▱▱", label: "50%", tone: "ok" });
  assert.equal(contextGauge(75).tone, "warning");
  assert.equal(contextGauge(95).tone, "error");
  assert.equal(contextGauge(150).label, "100%");
  assert.equal(contextGauge(undefined).label, "ctx ?");
});

test("a streaming message's tokens are estimated from its text, thinking, and tool arguments", () => {
  assert.equal(streamedTokens([{ type: "text", text: "x".repeat(400) }]), 100);
  assert.equal(streamedTokens([{ type: "thinking", thinking: "y".repeat(40) }, { type: "toolCall", arguments: { q: "abcd" } }]), 13);
  assert.equal(streamedTokens(undefined), 0);
  assert.equal(streamedTokens([{ type: "image" }]), 0);
});
