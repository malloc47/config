// The footer's numbers: token and cost totals over a session's entries, the context gauge, and
// paths under ~. Pure, so they're tested without pi; extensions/chrome.ts draws them.

type Usage = { input?: number; output?: number; cost?: { total?: number } };
type Entry = { type: string; usage?: Usage; message?: { role?: string; usage?: Usage } };

/** Input and output tokens and cost, from assistant messages, tool results, summaries, and usage entries. */
export function usageTotals(entries: readonly Entry[]): { input: number; output: number; cost: number } {
  let input = 0;
  let output = 0;
  let cost = 0;
  for (const entry of entries) {
    const usage = entry.type === "message" ? entry.message?.usage : entry.usage;
    if (!usage) continue;
    input += usage.input ?? 0;
    output += usage.output ?? 0;
    cost += usage.cost?.total ?? 0;
  }
  return { input, output, cost };
}

/** `950`, `9.5k`, `95k`, `1.2M`. */
export function formatTokens(count: number): string {
  if (count < 1000) return String(count);
  if (count < 10_000) return `${(count / 1000).toFixed(1)}k`;
  if (count < 1_000_000) return `${Math.round(count / 1000)}k`;
  return `${(count / 1_000_000).toFixed(1)}M`;
}

/** The working directory under ~. */
export function shortCwd(cwd: string, home: string | undefined): string {
  if (!home) return cwd;
  if (cwd === home) return "~";
  return cwd.startsWith(`${home}/`) ? `~${cwd.slice(home.length)}` : cwd;
}

/** Context use as an eight-cell bar and a percentage, with a tone: warning past 70%, error past 90%. */
export function contextGauge(percent: number | undefined): { bar: string; label: string; tone: "ok" | "warning" | "error" } {
  if (percent === undefined || !Number.isFinite(percent)) return { bar: "▱".repeat(8), label: "ctx ?", tone: "ok" };
  const clamped = Math.min(100, Math.max(0, percent));
  const filled = Math.round((clamped / 100) * 8);
  return {
    bar: "▰".repeat(filled) + "▱".repeat(8 - filled),
    label: `${Math.round(clamped)}%`,
    tone: clamped > 90 ? "error" : clamped > 70 ? "warning" : "ok",
  };
}
