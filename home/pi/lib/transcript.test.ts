import assert from "node:assert/strict";
import { test } from "node:test";

import { promptEcho, replyBullet } from "./transcript.ts";

test("a prompt is echoed after ❯; an envelope another package draws, and an empty one, are left alone", () => {
  assert.equal(promptEcho("where does @jdoe sit?"), "❯ where does @jdoe sit?");
  assert.equal(promptEcho("  two lines\nof prompt"), "❯ two lines\nof prompt");
  assert.equal(promptEcho('<inbound surface="slack" channel="D1">hi</inbound>'), '<inbound surface="slack" channel="D1">hi</inbound>');
  assert.equal(promptEcho("  <routine name=\"heartbeat\">"), "  <routine name=\"heartbeat\">");
  assert.equal(promptEcho("< 5 minutes left?"), "❯ < 5 minutes left?", "a less-than sign isn't a tag");
  assert.equal(promptEcho(""), "");
});

test("a reply opens with ⏺: in its first paragraph, in a heading, or above any other block", () => {
  assert.equal(replyBullet("Ryan sits on floor 36."), "⏺ Ryan sits on floor 36.");
  assert.equal(replyBullet("## Today\n\n- Q4 sync"), "## ⏺ Today\n\n- Q4 sync");
  assert.equal(replyBullet("- one\n- two"), "⏺\n\n- one\n- two");
  assert.equal(replyBullet("```ts\nx\n```"), "⏺\n\n```ts\nx\n```");
  assert.equal(replyBullet("| a | b |\n|---|---|"), "⏺\n\n| a | b |\n|---|---|");
  assert.equal(replyBullet("1. first"), "⏺\n\n1. first");
  assert.equal(replyBullet("**Bold** start"), "⏺ **Bold** start");
  assert.equal(replyBullet("   "), "   ");
});
