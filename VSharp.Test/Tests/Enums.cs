using System;
using NUnit.Framework;
using VSharp.Test.Tests.Methods;

namespace VSharp.Test.Tests
{
    [TestSvmFixture]
    public class Enums
    {
        public enum Color
        {
            Red
            , Blue
            , Yellow
        }

        [TestSvm]
        public static int Color2Int(Color c)
        {
            switch (c)
            {
                case Color.Blue:  return 500;
                case Color.Red:  return 10;
                default: return 42;
            }
        }

        [TestSvm]
        public static int SymbolicColor2Int(Color c, int v)
        {
            Color x = (Color) v;
            if (c == x)
            {
                return 42;
            }

            return 100;
        }

        [TestSvm]
        public static Type GetEnumType(Color c)
        {
            return c.GetType();
        }
    }
}