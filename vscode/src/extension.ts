import { workspace, ExtensionContext, Range, Uri, window } from "vscode";
import * as vscode from "vscode";
import child_process from "child_process";
import process, { env } from "process";
import path from "path";
import os from "os";

import {
  LanguageClient,
  LanguageClientOptions,
  Executable,
} from "vscode-languageclient/node";

// Default install location of Creusot
function getDefaultLSPPath(): string {
  if (process.platform == 'darwin') {
    return path.join(os.homedir(), '.creusot', '_opam', 'bin', "creusot-lsp");
  } else {
    return path.join(env.XDG_DATA_HOME || path.join(os.homedir(), ".local", "share"), "creusot", "_opam", "bin", "creusot-lsp");
  }
}

function creusotLspPath(): string {
   return workspace.getConfiguration("creusot").get("lspPath") || ""
}

function creusotHome(): string {
   return workspace.getConfiguration("creusot").get("home") || ""
}

function creusotDataHome(): string {
   return workspace.getConfiguration("creusot").get("dataHome") || ""
}

function creusotConfigHome(): string {
   return workspace.getConfiguration("creusot").get("configHome") || ""
}

function getLspPath(): string {
  const lspPath: string = creusotLspPath();
  return lspPath === "" ? getDefaultLSPPath() : lspPath;
}

function getServerExecutable(context): Executable {
  return { command: getLspPath() }
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
    if (vscode.workspace.workspaceFolders === undefined) {
      vscode.window.showErrorMessage("Could not run tests: no root path found");
      return;
    }
    const rootPath = vscode.workspace.workspaceFolders[0].uri.fsPath;
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
          const options: child_process.SpawnSyncOptions = {};
          const home = creusotHome();
          const dataHome = creusotDataHome();
          const configHome = creusotConfigHome();
          if (home !== "" || dataHome !== "" || configHome !== "") {
            const env = { ...process.env };
            if (home !== "") {
              env.HOME = home;
            }
            if (dataHome !== "") {
              env.XDG_DATA_HOME = dataHome;
            }
            if (configHome !== "") {
              env.XDG_CONFIG_HOME = configHome;
            }
            options.env = env;
          }
          // cargo creusot
          const buildOutput = child_process.spawnSync("cargo", ["creusot"], options);
          if (buildOutput.status !== 0) {
            const logs = buildOutput.stdout.toString() + "\n" + buildOutput.stderr.toString();
            run.failed(test, new vscode.TestMessage("Failed translation\n" + logs));
            return;
          }
          // why3find prove
          const output = child_process.spawnSync("cargo", ["creusot", "prove", test.id], options);
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
    process.chdir(rootPath);
    const output = child_process.spawnSync("cargo", ["creusot", "prove", test0.id, "-i"]);
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
    new vscode.ShellExecution("cargo", ["creusot", "prove"])
  )
  registerCommand(context, "creusot.prove", async () => {
    const exec = await vscode.tasks.executeTask(creusotProve)
  })

  const client = startServer(context);

  /* Virtual document to show Why3 proof context */
  const why3Scheme = 'why3';
  const why3DocProvider = new class implements vscode.TextDocumentContentProvider {
    // TODO: emit onDidChange events when proofs change
    onDidChangeEmitter = new vscode.EventEmitter<Uri>();
    onDidChange = this.onDidChangeEmitter.event;
    provideTextDocumentContent(uri: Uri): Thenable<string> {
      return client.sendRequest("creusot/showTask", uri.toString());
    }
  }
  context.subscriptions.push(vscode.workspace.registerTextDocumentContentProvider(why3Scheme, why3DocProvider));

  createTests(client);

  registerCommand(context, "creusot.stop", async () => {
    await client.stop();
  })

  registerCommand(context, "creusot.restart", async () => {
    await client.restart();
  })
}
