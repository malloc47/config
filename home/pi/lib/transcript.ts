// Claude Code's marks in the transcript: "❯ " before each of your prompts, as at the editor, and "⏺ "
// before each of the model's replies, as before its tool rows. Display only: Markdown transformers
// change what pi draws, never what the model gets. Pure, so they're tested without pi;
// extensions/transcript.ts registers them.

/** A line that opens a Markdown block (a heading, list, quote, fence, table, rule, or HTML) a mark can't go in front of. */
const BLOCK = /^(#{1,6}\s|[-*+]\s|\d+[.)]\s|>|```|~~~|\||---|\*\*\*|<)/;

/**
 * A prompt as you typed it, after "❯ ". A message that opens with a tag is left alone: it's an
 * envelope another package draws its own way (majordomo's cards for Slack and routines).
 */
export function promptEcho(markdown: string): string {
  if (!markdown.trim() || /^\s*<[A-Za-z]/.test(markdown)) return markdown;
  return `❯ ${markdown.replace(/^\s+/, "")}`;
}

/**
 * A reply after "⏺ ". A reply that opens with a heading takes the mark inside it, and one that opens
 * with another block (a list, a code fence, a table) gets the mark on a line of its own above it.
 */
export function replyBullet(markdown: string): string {
  const text = markdown.replace(/^\s+/, "");
  if (!text) return markdown;
  const heading = /^(#{1,6})\s+/.exec(text);
  if (heading) return `${heading[1]} ⏺ ${text.slice(heading[0].length)}`;
  if (BLOCK.test(text)) return `⏺\n\n${text}`;
  return `⏺ ${text}`;
}
