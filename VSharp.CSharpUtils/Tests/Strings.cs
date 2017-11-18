using System;
using System.Linq;

namespace VSharp.CSharpUtils.Tests
{
    public sealed class Strings
    {
//         Expecting HeapRef on empty string
//        public static string EmptyString()
//        {
//            return String.Empty;
//        }
//
//        public static string Interned()
//        {
//            String a = "interned";
//            return String.IsInterned(a);
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

//        public static bool EqualStrings()
//        {
//            string a = "i am working!";
//            string b = "i am ";
//            return a == (b + "working!");
//        }

//        public static bool NotEqualReferences()
//        {
//            string a = "i am working!";
//            string b = a;
//            a = "i am ";
//            return object.ReferenceEquals(a + "working!", b);
//        }
    }
}
