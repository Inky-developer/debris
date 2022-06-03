import * as vscode from 'vscode';

interface ParameterInfoShape {
	name: string;
	type: null | string;
	doc: string;
	modifier: string
}

interface BuiltinFunctionsShape {
	name: string;
	returnType: string;
	parameters: Array<ParameterInfoShape>;
	doc: string;
}

const builtinFunctions: Array<BuiltinFunctionsShape> = require("../res/functions.json");

export function activate(context: vscode.ExtensionContext) {
	vscode.languages.registerCompletionItemProvider("debris", {
		provideCompletionItems(document: vscode.TextDocument, position: vscode.Position, token: vscode.CancellationToken, context: vscode.CompletionContext) {
			return builtinFunctions.map((o) => {
				let item = new vscode.CompletionItem(o.name, vscode.CompletionItemKind.Function);
				let parameters: String[] = [];
				o.parameters.forEach(parameter => {
					const pType = parameter.type || "Any";
					parameters.push(parameter.name + ": " + pType);
				});
				item.detail = "fn " + o.name + "(" + parameters.join(", ") + ") -> " + o.returnType;
				item.commitCharacters = [];
				item.documentation = o.doc;
				return item;
			});
		}
	});
}

// this method is called when your extension is deactivated
export function deactivate() { }