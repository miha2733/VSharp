using System;
using System.IO;
using System.Collections.Generic;
using System.Globalization;
using System.Reflection;
using System.Security;
using System.Threading;
using VSharp.Interpreter;
using Microsoft.Z3;
using VSharp.Utils;

namespace VSharp.Runner
{
    internal class Runner
    {
        private static void PrepareSvm()
        {
            // Something like Propositional.ConfigureSimplifier(new Z3Simplifier()); can be used to enable Z3-based simplification (not recommended)
            SVM.ConfigureSolver(new SmtSolverWrapper<Microsoft.Z3.AST>(new Z3Solver()));
        }

        private static string _currentTestDirectory = "";

        private static Assembly LoadFromTestFolder(object sender, ResolveEventArgs args)
        {
            // This handler is called only when the common language runtime tries to bind to the assembly and fails.
            string name = new AssemblyName(args.Name).Name;
            string additionalPath = _currentTestDirectory + Path.DirectorySeparatorChar + name + ".dll";
            if (File.Exists(additionalPath))
            {
                return Assembly.LoadFrom(additionalPath);
            }

            return null;
        }

        public static void Main(string[] args)
        {
            CultureInfo ci = new CultureInfo("en-GB");
            ci.NumberFormat.PositiveInfinitySymbol = "Infinity";
            ci.NumberFormat.NegativeInfinitySymbol = "-Infinity";
            Thread.CurrentThread.CurrentCulture = ci;

            var toRun = args.Length > 1 ? args[1] : args[0];
            _currentTestDirectory = Path.GetDirectoryName(toRun);
            AppDomain currentDomain = AppDomain.CurrentDomain;
            currentDomain.AssemblyResolve += LoadFromTestFolder;
            PrepareSvm();

            Logger.current_log_level = args.Length > 1 ? Int32.Parse(args[0].Substring(3)) : Logger.Info;
            IDictionary<MethodInfo, string> got = Interpreter.SVM.Run(Assembly.LoadFile(toRun), new List<string> {});
        }
    }
}
