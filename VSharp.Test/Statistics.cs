using System;
using System.Collections;
using System.Collections.Generic;
using System.Diagnostics;
using System.Globalization;
using System.IO;
using System.Linq;
using System.Reflection;
using CsvHelper;
using Microsoft.FSharp.Core;

namespace VSharp.Test
{
    class ExceptionInfo : IComparable<ExceptionInfo>
    {
        public string Type { get; }
        public string Message { get; }
        public string Location { get; }
        public int Count { get; }
        public string ExampleMethod { get; }
        public ExceptionInfo(string type, string message, string location, int count, string exampleMethod)
        {
            Type = type;
            Message = message;
            Location = location;
            Count = count;
            ExampleMethod = exampleMethod;
        }

        public int CompareTo(ExceptionInfo other)
        {
            if (ReferenceEquals(this, other)) return 0;
            if (ReferenceEquals(null, other)) return 1;
            var typeComparison = string.Compare(Type, other.Type, StringComparison.Ordinal);
            if (typeComparison != 0) return typeComparison;
            var countComparison = Count.CompareTo(other.Count);
            if (countComparison != 0) return countComparison;
            return string.Compare(Message, other.Message, StringComparison.Ordinal);
        }
    }

    public class Statistics
    {
        private int _methodsNumber;
        private int _succeededMethodsNumber;
        private readonly Dictionary<string, Dictionary<string, Dictionary<string, List<string>>>> _allExceptions = new Dictionary<string, Dictionary<string, Dictionary<string, List<string>>>>(); // TODO: add three different groups (internal fail, ...)

        private List<ExceptionInfo> _exceptionInfos = null;

        private List<ExceptionInfo> ExceptionInfos
        {
            get { return _exceptionInfos ??= DictionaryToOrderedList(); }
        }
        // private readonly List<ExceptionInfo> _allExceptions = new List<ExceptionInfo>();

        public void SetupBeforeMethod(MethodBase m)
        {
            _methodsNumber++;
        }

        public void AddSucceededMethod(MethodBase m)
        {
            Console.WriteLine("DONE: {0}", m);
            _succeededMethodsNumber += 1;
        }

        private string MethodName(MethodBase m)
        {
            return $"{m.DeclaringType.FullName}.{m.Name}";
        }

        public void AddException(Exception e, MethodBase m)
        {
            var type = e.GetType().Name;
            var eMessage = e.Message;
            var frame = new StackTrace(e, true).GetFrame(0);
            var line = frame.GetFileLineNumber();
            var eLocation = $"{MethodName(frame.GetMethod())} at line {line}";

            // var eInfo = new ExceptionInfo(e.GetType().Name, e.Message, );
            if (!_allExceptions.ContainsKey(type))
            {
                _allExceptions[type] = new Dictionary<string,Dictionary<string, List<string>>>();
            }

            var typedExceptions = _allExceptions[type];
            if (!typedExceptions.ContainsKey(eMessage))
            {
                typedExceptions[eMessage] = new Dictionary<string, List<string>>();
            }

            var typedExceptionsWithMessage = typedExceptions[eMessage];
            if (!typedExceptionsWithMessage.ContainsKey(eLocation))
            {
                typedExceptionsWithMessage[eLocation] = new List<string>();
            }

            typedExceptionsWithMessage[eLocation].Add(MethodName(m));
        }

        private static int TotalCount(Dictionary<string, List<string>> dict)
        {
            int res = 0;
            foreach (var (key, list) in dict)
            {
                res += list.Count;
            }

            return res;
        }

        private static (int, string) SmartOrdering(string key)
        {
            if (key.Contains("Internal error"))
                return (1, key);
            return (0, key);
        }

        private List<ExceptionInfo> DictionaryToOrderedList()
        {
            var res = new List<ExceptionInfo>();
            foreach (var (type, typedExceptions) in _allExceptions)
                foreach (var (message, typedExceptionsWithMessage) in typedExceptions)
                    foreach (var (location, methods) in typedExceptionsWithMessage)
                        res.Add(new ExceptionInfo(type, message, location, methods.Count, methods.First()));
            res.Sort();
            return res;
        }

        public void SaveExceptionsShortStats()
        {
            var filename = Path.GetTempFileName();
            using var writer = new StreamWriter(filename);
            using var csv = new CsvWriter(writer, CultureInfo.InvariantCulture);
            csv.WriteHeader<ExceptionInfo>();
            csv.WriteRecords(ExceptionInfos);

            var format =
                new PrintfFormat<string, Unit, string, Unit>(
                        $"Exploration statistics has been saved in {filename}.");
            Logger.printLog(Logger.Info, format);
        }

        /*public void PrintExceptionsStats()
        {
            Console.WriteLine($"STATISTICS: Total methods number: {_methodsNumber}");
            Console.WriteLine($"STATISTICS: Succeeded methods number: {_succeededMethodsNumber}");
            Console.WriteLine("");
            Console.WriteLine("<-------------------------------------------------------------------------->");


            foreach (var exceptionInfo in ExceptionInfos)
            {

            }

            var orderedExceptions = _allExceptions.OrderBy(key => SmartOrdering(key.Key));
            foreach (var (eName, dict) in orderedExceptions)
            {
                Console.WriteLine("");
                Console.WriteLine($"STATISTICS: \"{eName}\" total number: {TotalCount(dict)}");
                Console.WriteLine("");
                var orderedDict = dict.OrderByDescending(key => key.Value.Count);
                foreach (var (location, methods) in orderedDict)
                {
                    Console.WriteLine($"STATISTICS: Occured in {location}");
                    Console.WriteLine($"STATISTICS: Number of occurrences: {methods.Count}");
                    Console.WriteLine($"STATISTICS: Method for debugging: {methods[0]}");
                    Console.WriteLine("");
                }
                Console.WriteLine("<-------------------------------------------------------------------------->");
            }
        }*/
    }
}
