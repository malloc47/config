// The editor's prompt, "❯ " as in Claude Code, drawn into the left padding of the input's first
// line, so the text and the cursor stay where pi's editor put them and longer input lines up under
// it. Pure, so it's tested without pi; extensions/chrome.ts draws it with pi's editor.

export const PROMPT = "❯";
/** The columns the prompt takes: the mark and a space, which the editor keeps as padding. */
export const PROMPT_WIDTH = 2;

/**
 * An editor's rendered lines with the prompt on the first line of input, the one after the top
 * border. A line that doesn't start with the padding (a terminal too narrow for it) is left alone.
 */
export function withPrompt(lines: string[], prompt: string): string[] {
  if (lines.length < 2 || !lines[1].startsWith(" ".repeat(PROMPT_WIDTH))) return lines;
  return [lines[0], prompt + lines[1].slice(PROMPT_WIDTH), ...lines.slice(2)];
}
