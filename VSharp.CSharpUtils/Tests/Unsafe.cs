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

        [StructLayout(LayoutKind.Explicit, Pack = 4, Size = 12)]
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

        [StructLayout(LayoutKind.Sequential, Pack=2)]
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

//        public static int StrangeSizeOf()
//        {
//            return sizeof(FixedSizedBuffer); // sizeof() = 70; Marshal.SizeOf() = 72; we should behave like sizeof()
//        }

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
            ExampleStruct kek = new ExampleStruct();
            ExampleStruct lol = new ExampleStruct();
            if (kek.Equals(lol)) {

            }
            return *newPtr;
        }

        public static int StrangeSizeOf()
        {
            return sizeof(ExampleStruct);
        }

        public static int StrangeSizeOf2()
        {
            return sizeof(ExampleStruct2);
        }

//        public static int ReturnConst()
//        {
//            int x = 421234123;
//            return *&x;
//        }
//
//        public static int DoubleIndirection()
//        {
//            int x = 428999;
//            int* p = &x;
//            return **&p;
//        }
//
//        public static int ReturnIntFromIntPtr(int myFavouriteParameter)
//        {
//            var s = new IntPtr(&myFavouriteParameter);
//            return *(int*) s.ToPointer();
//        }
//
//        public static void* CompilerHackLikePtrReturn(void* ptr)
//        {
//            var x = (IntPtr) ptr;
//            return x.ToPointer();
//        }
//
//        public static int SimplePointerDifference(int x, double y)
//        {
//            int* p = &x;
//            double* q = &y;
//            long d = (double*) p - q;
//
//            return * (int*) (q + d);
//        }
//
//        public static int PointerTriangle(int x, int y, int z)
//        {
//            int* px = &x;
//            int* py = &y;
//            int* pz = &z;
//
//            long d1 = px - py;
//            long d2 = py - pz;
//
//            int* r = pz + d1 + d2;
//
//            return *r; // x
//        }

//        public static long Reinterpret()
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
//
//        public static long GetHashCode(string str)
//        {
//            fixed (char* chPtr1 = str)
//            {
//                int num1 = 5381;
//                int num2 = num1;
//                char* chPtr2 = chPtr1;
//                int num3;
//                while ((num3 = (int) *chPtr2) != 0)
//                {
//                    num1 = (num1 << 5) + num1 ^ num3;
//                    int num4 = *(ushort*) ((IntPtr) chPtr2 + 2);
//                    if (num4 != 0)
//                    {
//                        num2 = (num2 << 5) + num2 ^ num4;
//                        chPtr2 += 2;
//                    }
//                    else
//                        break;
//                }
//                return num1 + num2 * 1566083941;
//            }
//        }
    }
}
