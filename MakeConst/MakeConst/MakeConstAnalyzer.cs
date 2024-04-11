using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using Microsoft.CodeAnalysis.Diagnostics;
using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;
using System.Threading;

namespace MakeConst
{
    [DiagnosticAnalyzer(LanguageNames.CSharp)]
    public class MakeConstAnalyzer : DiagnosticAnalyzer
    {
        public const string DiagnosticId = "MakeConst";

        // You can change these strings in the Resources.resx file. If you do not want your analyzer to be localize-able, you can use regular strings for Title and MessageFormat.
        // See https://github.com/dotnet/roslyn/blob/main/docs/analyzers/Localizing%20Analyzers.md for more on localization
        private static readonly LocalizableString Title = new LocalizableResourceString(nameof(Resources.AnalyzerTitle), Resources.ResourceManager, typeof(Resources));
        private static readonly LocalizableString MessageFormat = new LocalizableResourceString(nameof(Resources.AnalyzerMessageFormat), Resources.ResourceManager, typeof(Resources));
        private static readonly LocalizableString Description = new LocalizableResourceString(nameof(Resources.AnalyzerDescription), Resources.ResourceManager, typeof(Resources));
        private const string Category = "Usage";

        private static readonly DiagnosticDescriptor Rule = new DiagnosticDescriptor(DiagnosticId, Title, MessageFormat, Category, DiagnosticSeverity.Warning, isEnabledByDefault: true, description: Description);

        public override ImmutableArray<DiagnosticDescriptor> SupportedDiagnostics { get { return ImmutableArray.Create(Rule); } }

        public override void Initialize(AnalysisContext context)
        {
            context.ConfigureGeneratedCodeAnalysis(GeneratedCodeAnalysisFlags.None);
            context.EnableConcurrentExecution();

            context.RegisterSyntaxNodeAction(AnalyzeNode, SyntaxKind.LocalDeclarationStatement);
        }

        private void AnalyzeNode(SyntaxNodeAnalysisContext context)
        {
            // This cast always succeeds because the analyzer is only registered for local declarations.
            var localDeclaration = (LocalDeclarationStatementSyntax)context.Node;

            // make sure the declaration isn't already const:
            if (localDeclaration.Modifiers.Any(SyntaxKind.ConstKeyword))
            {
                return;
            }

            // Ensure that all variables in the local declaration have initializers that
            // are assigned with constant values.
            foreach (VariableDeclaratorSyntax variable in localDeclaration.Declaration.Variables)
            {
                EqualsValueClauseSyntax initializer = variable.Initializer;
                if (initializer == null)
                {
                    return;
                }

                Optional<object> constantValue = context.SemanticModel.GetConstantValue(initializer.Value, context.CancellationToken);
                if (!constantValue.HasValue)
                {
                    return;
                }
            }

            // Perform data flow analysis on the local declaration.
            DataFlowAnalysis dataFlowAnalysis = context.SemanticModel.AnalyzeDataFlow(localDeclaration);

            foreach (VariableDeclaratorSyntax variable in localDeclaration.Declaration.Variables)
            {
                // Retrieve the local symbol for each variable in the local declaration
                // and ensure that it is not written outside of the data flow analysis region.
                ISymbol variableSymbol = context.SemanticModel.GetDeclaredSymbol(variable, context.CancellationToken);
                if (dataFlowAnalysis.WrittenOutside.Contains(variableSymbol))
                {
                    return;
                }
            }

            // Raise the diagnostic
            context.ReportDiagnostic(Diagnostic.Create(Rule, context.Node.GetLocation(),
                localDeclaration.Declaration.Variables.First().Identifier.ValueText));
        }
    }
}
