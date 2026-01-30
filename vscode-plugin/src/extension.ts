import * as vscode from "vscode";
import {
  LanguageClient,
  LanguageClientOptions,
  ServerOptions,
  TransportKind,
} from "vscode-languageclient/node";

let client: LanguageClient | undefined;

function resolveServerPath(): string {
  const cfg = vscode.workspace.getConfiguration("kip");
  const configured = cfg.get<string>("languageServerPath") ?? "kip-lsp";
  return configured;
}

function resolveServerArgs(): string[] {
  const cfg = vscode.workspace.getConfiguration("kip");
  const args = cfg.get<string[]>("languageServerArgs") ?? [];
  return args;
}

function resolveTrace(): "off" | "messages" | "verbose" {
  const cfg = vscode.workspace.getConfiguration("kip");
  const trace = cfg.get<string>("trace.server") ?? "off";
  if (trace === "messages" || trace === "verbose") {
    return trace;
  }
  return "off";
}

export async function activate(context: vscode.ExtensionContext): Promise<void> {
  const outputChannel = vscode.window.createOutputChannel("Kip");

  const serverCommand = resolveServerPath();
  const serverArgs = resolveServerArgs();
  const serverOptions: ServerOptions = {
    command: serverCommand,
    args: serverArgs,
    transport: TransportKind.stdio,
    options: {
      env: process.env,
    },
  };

  const clientOptions: LanguageClientOptions = {
    documentSelector: [{ scheme: "file", language: "kip" }],
    outputChannel,
    initializationOptions: {},
    synchronize: {
      fileEvents: vscode.workspace.createFileSystemWatcher("**/*.kip"),
    },
    middleware: {
      provideDocumentFormattingEdits: (document, options, token, next) => {
        return next(document, options, token);
      },
    },
  };

  client = new LanguageClient(
    "kip-lsp",
    "Kip Language Server",
    serverOptions,
    clientOptions
  );

  client.trace = resolveTrace();

  context.subscriptions.push(outputChannel);
  context.subscriptions.push(client.start());

  context.subscriptions.push(
    vscode.workspace.onDidChangeConfiguration((event) => {
      if (
        event.affectsConfiguration("kip.languageServerPath") ||
        event.affectsConfiguration("kip.languageServerArgs") ||
        event.affectsConfiguration("kip.trace.server")
      ) {
        void restartClient();
      }
    })
  );
}

async function restartClient(): Promise<void> {
  if (!client) {
    return;
  }
  const newCommand = resolveServerPath();
  const newArgs = resolveServerArgs();
  const newTrace = resolveTrace();

  await client.stop();

  const serverOptions: ServerOptions = {
    command: newCommand,
    args: newArgs,
    transport: TransportKind.stdio,
    options: { env: process.env },
  };

  const clientOptions: LanguageClientOptions = {
    documentSelector: [{ scheme: "file", language: "kip" }],
    outputChannel: client.outputChannel,
    synchronize: {
      fileEvents: vscode.workspace.createFileSystemWatcher("**/*.kip"),
    },
  };

  client = new LanguageClient(
    "kip-lsp",
    "Kip Language Server",
    serverOptions,
    clientOptions
  );

  client.trace = newTrace;
  void client.start();
}

export async function deactivate(): Promise<void> {
  if (!client) {
    return;
  }
  await client.stop();
}
