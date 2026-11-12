import assert from "node:assert/strict";
import { test } from "node:test";

import { withPrompt } from "./prompt.ts";

const border = "────────────";

test("the prompt takes the first input line's padding, and the rest of the input lines up under it", () => {
  const lines = [border, "  ask about the Q4 deck  ", "  and the board meeting  ", border, "  /help  show commands"];
  assert.deepEqual(withPrompt(lines, "❯ "), [border, "❯ ask about the Q4 deck  ", "  and the board meeting  ", border, "  /help  show commands"]);
});

test("an empty editor gets the prompt before the cursor", () => {
  assert.deepEqual(withPrompt([border, "  \u001b[7m \u001b[0m  ", border], "> "), [border, "> \u001b[7m \u001b[0m  ", border]);
});

test("a line without the padding, as on a terminal too narrow for it, is left alone", () => {
  const narrow = [border, "x", border];
  assert.deepEqual(withPrompt(narrow, "❯ "), narrow);
  assert.deepEqual(withPrompt([border], "❯ "), [border]);
});
