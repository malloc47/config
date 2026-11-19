// tool-groups.ts and tool-rows.ts against pi's own tool rows, chained as pi chains them. This is
// what a pi bump is checked against: tool-groups.ts leans on two things pi's rows do that aren't
// documented as API (a self-drawn row whose renderers draw nothing takes no space, and a click
// reaches what the renderers drew before pi's own toggle), and these tests fail if either changes.
//
// Run by check.nix, which links pi's node_modules beside the package.

import assert from "node:assert/strict";
import { test } from "node:test";

import { Container, Text } from "@earendil-works/pi-tui";

import { ToolExecutionComponent } from "../node_modules/@earendil-works/pi-coding-agent/dist/modes/interactive/components/tool-execution.js";
import { initTheme } from "../node_modules/@earendil-works/pi-coding-agent/dist/modes/interactive/theme/theme.js";
import toolGroups from "./tool-groups.ts";
import toolRows from "./tool-rows.ts";

type Handler = (event: unknown, ctx?: unknown) => void;

initTheme("dark");
const resolvers: ((toolName: string, next: () => unknown) => unknown)[] = [];
const handlers = new Map<string, Handler[]>();
const pi = {
  registerToolRenderer: (resolver: (typeof resolvers)[number]) => resolvers.push(resolver),
  on: (event: string, handler: Handler) => handlers.set(event, [...(handlers.get(event) ?? []), handler]),
};
toolGroups(pi as never);
toolRows(pi as never);

/** The renderers pi would give a tool: the resolvers in load order, the first outermost. */
const renderersFor = (toolName: string) => {
  const step = (i: number): unknown => (i >= resolvers.length ? undefined : resolvers[i](toolName, () => step(i + 1)));
  return step(0);
};
const emit = (event: string, payload: unknown, ctx?: unknown) => {
  for (const handler of handlers.get(event) ?? []) handler(payload, ctx);
};

let expanded = false;
emit("session_start", {}, { mode: "tui", ui: { getToolsExpanded: () => expanded, notify() {} }, sessionManager: { getBranch: () => [] } });

const plain = (line: string) => line.replace(/\x1b\[[0-9;]*m|\x1b\][^\x07]*\x07|\x1b_[^\x07]*\x07/g, "").trimEnd();
const ui = { requestRender() {} };
const chat = new Container();
const rows = new Map<string, InstanceType<typeof ToolExecutionComponent>>();

const call = (id: string, name: string) => ({ type: "toolCall", id, name, arguments: { id } });
/** An assistant message's calls, as pi adds their rows while it streams, then the message ending. */
function assistant(...content: unknown[]): void {
  const message = { role: "assistant", content };
  emit("message_update", { message });
  for (const part of content as { type: string; id: string; name: string; arguments: unknown }[]) {
    if (part.type !== "toolCall") continue;
    const row = new ToolExecutionComponent(part.name, part.id, part.arguments, { outputPad: 1 }, renderersFor(part.name) as never, ui as never, "/home/u");
    row.setExpanded(expanded);
    chat.addChild(row);
    rows.set(part.id, row);
  }
  emit("message_end", { message });
}
function finish(id: string, text: string, isError = false): void {
  rows.get(id)?.updateResult({ content: [{ type: "text", text }], isError, durationMs: 10 } as never, false);
  emit("message_end", { message: { role: "toolResult", toolCallId: id, content: [] } });
}
const view = () => chat.render(100).map(plain);
function click(id: string, x: number): unknown {
  const row = rows.get(id);
  assert.ok(row);
  // A row draws a blank line, then what its renderers drew: the run's line is at y 1.
  return row.handleMouse({ type: "click", button: "left", x, y: 1, screenX: x, screenY: 1, width: 100, height: row.render(100).length, shift: false, alt: false, ctrl: false } as never);
}

chat.addChild(new Text("THINKING", 0, 0));
assistant(call("a", "mcp__slack__slack_read_channel"), call("b", "mcp__ms365__list_mail_messages"));
finish("a", "12 messages");
finish("b", "5 mails");
chat.addChild(new Text("THINKING", 0, 0));
assistant({ type: "thinking", thinking: "…" }, call("c", "mcp__servicenow__get_reservation"));
finish("c", "Reservation R1");
assistant(call("d", "bash"));
finish("d", "Wed Nov 19");
assistant(call("e", "mcp__slack__slack_read_thread"));
finish("e", "not_in_channel", true);

test("a run draws one line in its first row, and its other rows take no space", () => {
  assert.deepEqual(view().slice(0, 4), ["THINKING", "", " ⏺ Tools: Slack(read channel), Ms365(list mail messages), Servicenow(get reservation)", "THINKING"]);
  assert.deepEqual(rows.get("b")?.render(100), [], "a grouped row after the first draws nothing, not even its blank line");
  assert.deepEqual(rows.get("c")?.render(100), []);
});

test("an ungrouped tool keeps its row and ends the run, and a failed call opens on its own", () => {
  const lines = view();
  assert.ok(lines.includes(" ⏺ Bash(date)") || lines.some((l) => l.startsWith(" ⏺ Bash(")), lines.join("\n"));
  const failed = lines.indexOf(" ⏺ Tools: Slack(read thread)");
  assert.ok(failed > 0, lines.join("\n"));
  assert.match(lines[failed + 1], /^ ⏺ slack - slack_read_thread \(MCP\)/);
  assert.match(lines[failed + 2], /not_in_channel/);
});

test("clicking an item opens its row, and the line keeps clicks between items", () => {
  const line = view()[2];
  const result = click("a", line.indexOf("Ms365") + 1) as { handled?: boolean } | undefined;
  assert.equal(result?.handled, true, "the click reaches the line before pi's toggle of the row");
  assert.match(rows.get("b")?.render(100).map(plain).join("\n") ?? "", /ms365 - list_mail_messages \(MCP\)[\s\S]*5 mails/);
  assert.deepEqual(rows.get("c")?.render(100), [], "only the clicked call opens");
  click("a", line.indexOf("Ms365") + 1);
  assert.deepEqual(rows.get("b")?.render(100), [], "a second click closes it");
  const gap = click("a", line.indexOf("), ") + 2) as { handled?: boolean } | undefined;
  assert.equal(gap?.handled, true);
  assert.equal(rows.get("a")?.render(100).length, 2, "a click on the space between items opens nothing, and pi doesn't toggle the row");
});

test('clicking "Tools:" opens every call in the run, and again closes them', () => {
  click("a", 3);
  for (const id of ["a", "b", "c"]) assert.ok((rows.get(id)?.render(100).length ?? 0) > 2, id);
  click("a", 3);
  assert.deepEqual(rows.get("b")?.render(100), []);
  assert.equal(rows.get("a")?.render(100).length, 2);
});

test("ctrl+o shows every call's row as usual", () => {
  expanded = true;
  for (const row of rows.values()) row.setExpanded(true);
  const lines = view();
  assert.ok(!lines.some((l) => l.includes("Tools:")), lines.join("\n"));
  for (const tool of ["slack_read_channel", "list_mail_messages", "get_reservation"]) assert.ok(lines.some((l) => l.includes(tool)), tool);
  expanded = false;
  for (const row of rows.values()) row.setExpanded(false);
  assert.deepEqual(rows.get("b")?.render(100), []);
});
