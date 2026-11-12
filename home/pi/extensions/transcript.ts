// Claude Code's marks in the transcript (lib/transcript.ts): "❯ " before your prompts and "⏺ "
// before the model's replies. Display only; the model gets its messages unchanged. pi runs
// transformers in load order, and this package loads before majordomo's, whose cards draw the
// envelopes this leaves alone.

import type { ExtensionAPI } from "@earendil-works/pi-coding-agent";

import { promptEcho, replyBullet } from "../lib/transcript.ts";

export default function transcript(pi: ExtensionAPI): void {
  // A throw while loading would stop pi from starting, majordomo's desk included.
  try {
    pi.registerMarkdownTransformer((markdown, { messageType }) => (messageType === "user" ? promptEcho(markdown) : messageType === "assistant" ? replyBullet(markdown) : markdown), {
      messageTypes: ["user", "assistant"],
    });
  } catch (error) {
    console.error(`pi-personal: transcript marks are off: ${String(error)}`);
  }
}
