import { workspace, ExtensionContext, Range, Uri, window } from "vscode";
import * as vscode from "vscode";
import child_process from "child_process";
import process from "process";

import {
  LanguageClient,
  LanguageClientOptions,
  Executable,
} from "vscode-languageclient/node";

function getServerExecutable(context): Executable {
  const lspPath: string | undefined = workspace.getConfiguration("creusot").get("lspPath");
  if (lspPath === undefined || lspPath === "") {
    return { command: "creusot-lsp" };
  } else {
    return { command: lspPath };
  }
}

const languages = [
  { scheme: "file", language: "rust" },
  { scheme: "file", language: "coma" },
  { scheme: "file", language: "why3session" },
  { scheme: "file", language: "why3proof" },
];

function startServer(context): LanguageClient {
  const outputChannel = vscode.window.createOutputChannel("Creusot IDE");
  const traceOutputChannel = vscode.window.createOutputChannel("Creusot IDE Trace");
  const clientOptions: LanguageClientOptions = {
    outputChannel,
    traceOutputChannel,
    documentSelector: languages,
    synchronize: {},
  };
  const client = new LanguageClient("creusot-ide", "Creusot IDE", getServerExecutable(context), clientOptions);
  client.start();
  return client;
}

type RawPosition = { line: number, character: number }
type RawRange = { start: RawPosition, end: RawPosition }
type RawLocation = { uri: string, range: RawRange }
type RawItem = { id: string, label: string, range: RawRange }

const mkPosition = (raw: RawPosition) => new vscode.Position(raw.line, raw.character)
const mkRange = (raw: RawRange) => new vscode.Range(mkPosition(raw.start), mkPosition(raw.end))
const mkLocation = (raw: RawLocation) => new vscode.Location(Uri.parse(raw.uri), mkRange(raw.range))

function registerCommand(context: ExtensionContext, name: string, cmd: (...args: any[]) => any) {
  const disposable = vscode.commands.registerCommand(name, cmd)
  context.subscriptions.push(disposable)
}

function getExtension(fname: string): string {
  return fname.slice((fname.lastIndexOf(".") - 1 >>> 0) + 2);
}

async function createTests(client: LanguageClient) {
  let x = 0;
  const controller = vscode.tests.createTestController(
    'creusotProofs',
    'Creusot Proofs'
  );

  function addTestItems(rawUri: string, items: RawItem[]) {
    console.log("addTestItems", rawUri, items);
    let uri = Uri.parse(rawUri);
    const fileItem = controller.createTestItem(uri.toString(), uri.fsPath, uri);
    controller.items.add(fileItem);
    for (const item of items) {
      const test = controller.createTestItem(item.id, item.label, uri);
      test.range = mkRange(item.range);
      fileItem.children.add(test);
    }
  }
  client.onNotification("creusot/testitems", addTestItems);

  async function runHandler(request: vscode.TestRunRequest, cancellation: vscode.CancellationToken) {
    const rootPath = vscode.workspace.rootPath;
    if (rootPath === undefined) {
      vscode.window.showErrorMessage("Could not run tests: no root path found");
      return;
    }
    const run = controller.createTestRun(request, "Prove");
    let requests: readonly vscode.TestItem[];
    if (request.include) {
      requests = request.include
    } else {
      let tmp: vscode.TestItem[] = [];
      for (const i of controller.items) {
        tmp.push(i[1]);
      }
      requests = tmp;
    };
    for (const test of requests) {
      if (cancellation.isCancellationRequested) {
        break;
      }
      if (getExtension(test.id) === "coma") {
        async function runTest(test: vscode.TestItem, rootPath: string) {
          run.enqueued(test);
          run.started(test);
          run.appendOutput(`Starting test ${test.label}\n\r`);
          // Save the rust file
          if (test.uri) { await vscode.workspace.save(test.uri); }
          process.chdir(rootPath);
          // cargo creusot
          const buildOutput = child_process.spawnSync("cargo", ["creusot"]);
          if (buildOutput.status !== 0) {
            const logs = buildOutput.stdout.toString() + "\n" + buildOutput.stderr.toString();
            run.failed(test, new vscode.TestMessage("'cargo creusot' failed\n" + logs));
            return;
          }
          // why3find prove
          const output = child_process.spawnSync("why3find", ["prove", test.id]);
          run.appendOutput(`Finishing test ${test.label}\n\r`);
          if (output.status !== 0) {
            const logs = output.stdout.toString() + "\n" + output.stderr.toString();
            run.failed(test, new vscode.TestMessage("Failed proof\n" + logs));
            return;
          }
          run.passed(test);
        }
        await runTest(test, rootPath);
      }
    }
    run.end();
  }
  const runProfile = controller.createRunProfile("Run", vscode.TestRunProfileKind.Run, runHandler);
  async function debugHandler(request: vscode.TestRunRequest, cancellation: vscode.CancellationToken) {
    const rootPath = vscode.workspace.rootPath;
    if (rootPath === undefined) {
      vscode.window.showErrorMessage("Could not debug tests: no root path found");
      return;
    }
    if (!request.include) { return; }
    const test0 = request.include[0];
    const run = controller.createTestRun(request, "Launch IDE", false);
    run.started(test0);
    const output = child_process.spawnSync("why3find", ["prove", "--root", rootPath, test0.id, "-i"]);
    run.end();
  }
  const debugProfile = controller.createRunProfile("Debug", vscode.TestRunProfileKind.Debug, debugHandler);
}

export function activate(context: ExtensionContext) {
  /* Basic system commands */
  registerCommand(context, "creusot.openFile", async (file) => {
    const uri = Uri.file(file);
    const document = await workspace.openTextDocument(uri);
    await window.showTextDocument(document);
  })
  registerCommand(context, "creusot.peekLocations", async (rawUri, rawPosition, rawLocations: RawLocation[]) => {
    const uri = Uri.parse(rawUri)
    const position = mkPosition(rawPosition)
    const locations = rawLocations.map(mkLocation)
    await vscode.commands.executeCommand("editor.action.peekLocations", uri, position, locations, "peek")
  })
  /* Creusot Build */
  const creusotBuild = new vscode.Task(
    { type: "shell" },
    vscode.TaskScope.Workspace,
    "Creusot Build",
    "creusot",
    new vscode.ShellExecution("cargo", ["creusot"])
  )
  registerCommand(context, "creusot.build", async () => {
    const exec = await vscode.tasks.executeTask(creusotBuild)
  })
  /* Launch Why3 IDE */
  const why3ide = new vscode.Task(
    {
      type: "shell",
      command: "cargo",
      args: ["creusot", "why3", "ide"]
    },
    vscode.TaskScope.Workspace,
    "Launch Why3 IDE",
    "creusot",
    new vscode.ShellExecution("cargo", ["creusot", "why3", "ide"])
  )
  registerCommand(context, "creusot.why3ide", async () => {
    const exec = await vscode.tasks.executeTask(why3ide)
  })

  /* Why3find Prove */
  const creusotProve = new vscode.Task(
    {
      type: "shell",
      command: "why3find",
    },
    vscode.TaskScope.Workspace,
    "Why3find Prove Everything",
    "creusot",
    new vscode.ShellExecution("why3find", ["prove", "--package", "prelude", "verif/"])
  )
  registerCommand(context, "creusot.prove", async () => {
    const exec = await vscode.tasks.executeTask(creusotProve)
  })

  const client = startServer(context);

  /* Virtual document to show Why3 proof context */
  const why3Scheme = 'why3';
  const why3DocProvider = new class implements vscode.TextDocumentContentProvider {
    onDidChangeEmitter = new vscode.EventEmitter<Uri>();
    onDidChange = this.onDidChangeEmitter.event;
    content = "";
    newText(newContent) {
      this.content = newContent;
    }
    provideTextDocumentContent(uri: Uri): string {
      return this.content;
    }
  }
  context.subscriptions.push(vscode.workspace.registerTextDocumentContentProvider(why3Scheme, why3DocProvider));
  const virtualFileName = "Task";
  const uri = vscode.Uri.parse('why3:' + virtualFileName);
  registerCommand(context, "creusot.showTask", async (arg) => {
    const msg = client.sendRequest("creusot/show", arg);
    why3DocProvider.newText(msg);
    why3DocProvider.onDidChangeEmitter.fire(uri);
    const doc = await vscode.workspace.openTextDocument(uri);
    vscode.languages.setTextDocumentLanguage(doc, "coma");
    await vscode.window.showTextDocument(doc, { preview: false });
  });

  createTests(client);
}
