using System;

namespace VSharp.CSharpUtils.Tests
{
    public sealed class Strings
    {
        // Expecting HeapRef on empty string
        public static string EmptyString(int n, int m)
        {
            return String.Empty;
        }

        public static string Interned()
        {
            String a = "interned";
            return String.IsInterned(a);
        }

        public static string Intern()
        {
            string a = new string(new char[] {'a', 'b', 'c'});
            return String.Intern(a);
        }

        public static Object NotInterned()
        {
            string a = new string(new char[] {'a', 'b', 'c'});
            return String.IsInterned(a);
        }

        public static string NotSurelyInterned(string a)
        {
            return String.IsInterned(a);
        }

        public static string NotSurelyIntern(string a)
        {
            return String.Intern(a);
        }

        public static void TestHeap(int l, string a)
        {
            if (l > 0)
                String.Intern(a);
        }
    }
}
