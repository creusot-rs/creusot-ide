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

const server : Executable = {
    command: "/home/sam/rust/cide/_build/default/server/bin/main.exe",
}

const languages = [
    { scheme: "file", language: "rust" },
    { scheme: "file", language: "coma" },
    { scheme: "file", language: "why3session" },
];

function startServer() : LanguageClient {
    const outputChannel = vscode.window.createOutputChannel("Creusot IDE");
    const traceOutputChannel = vscode.window.createOutputChannel("Creusot IDE Trace");
    const clientOptions : LanguageClientOptions = {
        outputChannel,
        traceOutputChannel,
        documentSelector: languages,
        synchronize: {},
    };
    const client = new LanguageClient("creusot-ide", "Creusot IDE", server, clientOptions);
    client.start();
    return client;
}

export function activate(context) {
    const client = startServer();
    const disposable = vscode.commands.registerCommand("creusot.openFile", async (file) => {
        const uri = Uri.file(file);
        const document = await workspace.openTextDocument(uri);
        await window.showTextDocument(document);
    })
    context.subscriptions.push(disposable);
}
