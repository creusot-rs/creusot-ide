import { workspace, ExtensionContext, Range, Uri, window } from "vscode";
import * as vscode from "vscode";
import * as os from "os";

import {
    DocumentUri,
    LanguageClient,
    LanguageClientOptions,
    NotificationType,
    RequestType,
    ServerOptions,
    Executable,
} from "vscode-languageclient/node";
import { access, existsSync } from "fs";
import { runInContext } from "vm";

function getServerExecutable(context) : Executable {
    const lspPath : string | undefined = workspace.getConfiguration("creusot").get("lspPath");
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

function startServer(context) : LanguageClient {
    const outputChannel = vscode.window.createOutputChannel("Creusot IDE");
    const traceOutputChannel = vscode.window.createOutputChannel("Creusot IDE Trace");
    const clientOptions : LanguageClientOptions = {
        outputChannel,
        traceOutputChannel,
        documentSelector: languages,
        synchronize: {},
    };
    const client = new LanguageClient("creusot-ide", "Creusot IDE", getServerExecutable(context), clientOptions);
    client.start();
    return client;
}

type RawPosition = {line: number, character: number}
type RawRange = {start: RawPosition, end: RawPosition}
type RawLocation = {uri: string, range: RawRange}

const mkPosition = (raw : RawPosition) => new vscode.Position(raw.line, raw.character)
const mkRange = (raw : RawRange) => new vscode.Range(mkPosition(raw.start), mkPosition(raw.end))
const mkLocation = (raw : RawLocation) => new vscode.Location(Uri.parse(raw.uri), mkRange(raw.range))

function registerCommand(context: ExtensionContext, name: string, cmd: (...args: any[]) => any) {
    const disposable = vscode.commands.registerCommand(name, cmd)
    context.subscriptions.push(disposable)
}

export function activate(context: ExtensionContext) {
    /* Basic system commands */
    registerCommand(context, "creusot.openFile", async (file) => {
        const uri = Uri.file(file);
        const document = await workspace.openTextDocument(uri);
        await window.showTextDocument(document);
    })
    registerCommand(context, "creusot.peekLocations", async (rawUri, rawPosition, rawLocations : RawLocation[]) => {
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
            "args": ["prove", "--package", "prelude", "target/*.coma"]
        },
        vscode.TaskScope.Workspace,
        "Why3find Prove",
        "creusot",
        new vscode.ShellExecution("why3find", ["prove", "--package", "prelude", "target/*.coma"])
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
        provideTextDocumentContent(uri: Uri) : string {
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
}
