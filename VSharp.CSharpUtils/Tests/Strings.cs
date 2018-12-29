using System;

namespace VSharp.CSharpUtils.Tests
{
    public sealed class Strings
    {
        // Expecting HeapRef on empty string
//        public static string EmptyString(int n, int m)
//        {
//            return String.Empty;
//        }
//
//        public static string SymbolicString(string s)
//        {
//            var len = s.Length;
//            return s;
//        }
//
//        public static string HopHeyCharArray(char[] a)
//        {
//            return new string(a);
//        }
//
//        public static int NullLength()
//        {
//            string s = null;
//            return s.Length;
//        }
//
//        public static int GetHash(string s)
//        {
//            var interned = String.Intern(s);
//            return interned.GetHashCode();
//        }

//        public static string ConcreteIsInterned()
//        {
//            String a = "interned";
//            return String.IsInterned(a);
//        }
//
//        public static string ConcreteIntern()
//        {
//            string a = new string(new char[] {'a', 'b', 'c'});
//            return String.Intern(a);
//        }
//
//        public static Object NotInterned()
//        {
//            string a = new string(new char[] {'a', 'b', 'c'});
//            return String.IsInterned(a);
//        }

        public static string NotSurelyInterned(string a)
        {
            return String.IsInterned(a);
        }

        public static string NotSurelyIntern(string a)
        {
            return String.Intern(a);
        }
    }
}
