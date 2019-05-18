using System;
using System.Runtime.InteropServices;

namespace VSharp.CSharpUtils.Tests
{
    public unsafe class Unsafe
    {
//        public static int ChangeThroughIndirection()
//        {
//            int x = 42;
//            int z = 14;
//            *&x = *&z;
//            return x; // 14
//        }

//        public static int CharSizeOf()
//        {
//            return sizeof(char); // sizeof() = 2; Marshal.SizeOf() = 1; we should be 2
//        }
        public class TestBase
        {
            public byte bA = 0xaa;
            public byte bB = 0xbb;
            public byte bC = 0xcc;
            public Int64 bD = 0xdd;
            public const byte b1 = 0xee;
            public static byte bStatics = 0xfe;
            public static byte bStatics2 = 0xef;
        }

        [StructLayout(LayoutKind.Explicit, Pack = 4, Size = 100)]
        struct FixedSizedBuffer
        {
            [FieldOffset(4)]
            public fixed char buf[20];
            [FieldOffset(6)]
            public fixed bool bufs[29];
        }

        [StructLayout(LayoutKind.Sequential, Pack = 2)]
        struct FixedSizedBuffer2
        {
            public fixed char buf[20];
            public fixed bool bufs[29];
            public long number;
        }

        [StructLayout(LayoutKind.Sequential, Pack = 2)]
        struct ExampleStruct
        {
            public byte b1;
            public byte b2;
            public int i3;
        }

        [StructLayout(LayoutKind.Sequential, Pack=0)]
        struct ExampleStruct2
        {
            public byte b1;
            public byte b2;
            public int i3;
        }

        [StructLayout(LayoutKind.Sequential, Pack=128)]
        struct ExampleStruct3
        {
            public byte b1;
            public byte b2;
            public int i3;
        }

        // Size of this: 18 bytes
        [StructLayout(LayoutKind.Sequential, Pack = 4)]
        struct ExampleStruct4
        {
            //public const byte b1 = 0xaa;
            public byte b2;
            public byte i3;
            public byte i;
            public fixed byte ar[15];
        }

        public static int StrangeSizeOf()
        {
            return sizeof(FixedSizedBuffer); // sizeof() = 70; Marshal.SizeOf() = 72; we should behave like sizeof()
        }

        public static int StrangeSizeOf2()
        {
            return sizeof(ExampleStruct);
        }

        public static int StrangeSizeOf3()
        {
            return sizeof(ExampleStruct2);
        }

        public static char* GetBuffer()
        {
            FixedSizedBuffer Struct = new FixedSizedBuffer();
            return Struct.buf;
        }

        public static byte FeelOffset()
        {
            ExampleStruct Struct = new ExampleStruct();
            ExampleStruct* ptr = &Struct;
            byte* newPtr = (byte*)ptr;
            newPtr++;
            return *newPtr;
        }

//        public static bool EqualityOfStructs() <- TODO: doesn't work yet
//        {
//            ExampleStruct struct1 = new ExampleStruct();
//            ExampleStruct struct2 = new ExampleStruct();
//            return struct1.Equals(struct2);
//        }

//        public static int makeDangerousStruct()
//        {
//            ExampleStruct2 s = new ExampleStruct2();
//            return 0;
//        }

        public static int makeDangerousStruct2()
        {
            ExampleStruct4 s = new ExampleStruct4();
            return 0;
        }

        public static int ReturnConst()
        {
            int x = 421234123;
            return *&x;
        }

        public static int DoubleIndirection()
        {
            int x = 428999;
            int* p = &x;
            return **&p;
        }

        public static int ReturnIntFromIntPtr(int myFavouriteParameter)
        {
            var s = new IntPtr(&myFavouriteParameter);
            return *(int*) s.ToPointer();
        }

        public static void* CompilerHackLikePtrReturn(void* ptr)
        {
            var x = (IntPtr) ptr;
            return x.ToPointer();
        }

        public static int SimplePointerDifference(int x, double y)
        {
            int* p = &x;
            double* q = &y;
            long d = (double*) p - q;

            return * (int*) (q + d);
        }

        public static int PointerTriangle(int x, int y, int z)
        {
            int* px = &x;
            int* py = &y;
            int* pz = &z;

            long d1 = px - py;
            long d2 = py - pz;

            int* r = pz + d1 + d2;

            return *r; // x
        }

//        public static long Reinterpret() <- TODO: dotPeek can not... Kostya, hurry up...
//        {
//            long[] test = { 18L, 19L };
//            fixed (long* p = test)
//            {
//                int* intPtr = (int*)p;
//                intPtr += 1;
//                long* longPtr = (long*)intPtr;
//                return *longPtr;
//            }
//        }

        public static long Reinterpret() // TODO: check ast
        {
            long[] test = { 18L, 19L };
            fixed (long* p = &test[0])
            {
                int* intPtr = (int*)p;
                intPtr += 1;
                long* longPtr = (long*)intPtr;
                return *longPtr;
            }
        }

        public static int Reinterpret2()
        {
            byte[] test = { 0xaa, 0xbb };
            fixed (byte* p = &test[0])
            {
                int* intPtr = (int*)p;
                return *intPtr;
            }
        }

        public static long GetHashCode(string str)
        {
            fixed (char* chPtr1 = str)
            {
                int num1 = 5381;
                int num2 = num1;
                char* chPtr2 = chPtr1;
                int num3 = *chPtr2;
                while (num3 != 0)
                {
                    num1 = (num1 << 5) + num1 ^ num3;
                    int num4 = *(ushort*) ((IntPtr) chPtr2 + 2);
                    if (num4 != 0)
                    {
                        num2 = (num2 << 5) + num2 ^ num4;
                        chPtr2 += 2;
                    }
                    else
                        break;
                    num3 = *chPtr2;
                }
                return num1 + num2 * 1566083941;
            }
        }
    }
}
