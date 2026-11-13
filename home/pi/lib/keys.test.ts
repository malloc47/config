import assert from "node:assert/strict";
import { test } from "node:test";

import { validKey } from "./keys.ts";

test("keys read as pi's key syntax", () => {
  for (const key of ["ctrl+s", "alt+s", "ctrl+alt+s", "shift+f2", "ctrl+/", "ctrl+\\", "alt+pageDown"]) assert.ok(validKey(key), key);
  for (const key of ["", "s+ctrl", "ctrl+ctrl+s", "hyper+s", "ctrl+", "ctrl+ss"]) assert.ok(!validKey(key), key);
});
