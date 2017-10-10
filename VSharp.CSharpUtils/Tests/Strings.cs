using System;
using System.Linq;

namespace VSharp.CSharpUtils.Tests
{
    public sealed class Strings
    {
//         Expecting HeapRef on empty string
        public static string EmptyString()
        {
            return String.Empty;
        }

        public static string ObjString()
        {
            String a = "interned";
            return String.IsInterned(a);
        }

//        public static Object ObjString1()
//        {
//            string a = new string(new char[] {'a', 'b', 'c'});
//            return String.IsInterned(a);
//        }
    }
}
