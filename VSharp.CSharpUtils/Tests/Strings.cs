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

        public static string StringOfCharArray(char[] a)
        {
            return new string(a);
        }

        public static int EmptyString1(char[] a)
        {
            char[] t = null;
            string s = new string(t);
            return s.Length;
        }

        public static string HopHeyCharArray()
        {
            char[] a = {'a', 'b', 'c'};
            return new string(a);
        }

        public static int NullLength()
        {
            string s = null;
            return s.Length;
        }
    }
}
