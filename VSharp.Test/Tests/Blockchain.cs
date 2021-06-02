using System.Collections.Generic;
using NBlockchain;
using NBlockchain.Models;
using NBlockchain.Services;
using System;
using System.Collections.Generic;
using System.Text;
using NBlockchain.Models;
using NBlockchain.Interfaces;
using NBlockchain.Services.PeerDiscovery;

using System;
using System.Collections.Generic;
using System.Text;
using Microsoft.Extensions.DependencyInjection; //LoggingServiceCollectionExtensions
using Microsoft.Extensions.DependencyInjection;
using Microsoft.Extensions.Logging;
// using NBlockchain.Tests.Scenarios.Common;
// using Xunit;
using NBlockchain.Services.PeerDiscovery;
using NBlockchain.Models;
using NBlockchain.Interfaces;
using System.Threading.Tasks;

namespace VSharp.Test.Tests
{
    [TestSvmFixture]
    public class Blockchain
    {
        [InstructionType("txn-v1")]
        public class TestInstruction : Instruction
        {
            public string Data { get; set; }

            public override ICollection<byte[]> ExtractSignableElements()
            {
                return new List<byte[]>() { Encoding.UTF8.GetBytes(Data) };
            }

            public override int GetHashCode()
            {
                return Data.GetHashCode();
            }
        }

        // public class TestInstruction : Instruction
        // {
        //     public string Data { get; set; }
        //
        //     public override ICollection<byte[]> ExtractSignableElements()
        //     {
        //         return new List<byte[]>() { Encoding.UTF8.GetBytes(Data) };
        //     }
        //
        //     public override int GetHashCode()
        //     {
        //         return Data.GetHashCode();
        //     }
        // }

        class BaseBuilder : BlockbaseTransactionBuilder
        {
            public BaseBuilder(IAddressEncoder addressEncoder, ISignatureService signatureService, ITransactionBuilder transactionBuilder)
                : base(addressEncoder, signatureService, transactionBuilder)
            {
            }

            protected override ICollection<Instruction> BuildInstructions(KeyPair builderKeys, ICollection<Transaction> transactions)
            {
                var instructions = new HashSet<Instruction>();
                var i1 = new TestInstruction();
                i1.Data = "test";
                i1.PublicKey = builderKeys.PublicKey;
                SignatureService.SignInstruction(i1, builderKeys.PrivateKey);
                instructions.Add(i1);

                return instructions;
            }
        }

        private static Block GenerateBlock(byte[] id, byte[] prevBlock, uint height)
        {
            var block = new Block();
            block.Header.BlockId = id;
            block.Header.Height = height;
            block.Header.Status = BlockStatus.Confirmed;
            //block.Header.Timestamp
            block.Header.PreviousBlock = prevBlock;
            return block;
        }


        public static void PopulateInitialData(IBlockRepository repo)
        {
            var prevBlock = new byte[0];
            for (byte i = 0; i < 1; i++)
            {
                var block = GenerateBlock(new byte[] { i }, prevBlock, i);
                repo.AddBlock(block);
                prevBlock = block.Header.BlockId;
            }
        }

        private static IServiceProvider ConfigureNode(uint port, ICollection<string> peers)
        {
            IServiceCollection services = new ServiceCollection();
            services.AddBlockchain(x =>
            {
                x.UseTcpPeerNetwork(port);
                x.AddPeerDiscovery(sp => new StaticPeerDiscovery(peers));
                // x.AddInstructionType<TestInstruction>();
                x.UseBlockbaseProvider<BaseBuilder>();
                x.UseParameters(new StaticNetworkParameters()
                {
                BlockTime = TimeSpan.FromSeconds(10),
                HeaderVersion = 1
                });
            });

            var serviceProvider = services.BuildServiceProvider();
            return serviceProvider;
            // return null;
        }

        [TestSvm]
        public static void  should_sync_data_over_mesh()
        {
            uint port1 = 1001;//Helpers.GetFreePort();
            uint port2 = 1002; //Helpers.GetFreePort();
            uint port3 = 1003; //Helpers.GetFreePort();
            var node1 = ConfigureNode(port1, new string[0]);
            // var node2 = ConfigureNode(port2, new string[] { $"tcp://localhost:{port1}" });
            // var node3 = ConfigureNode(port3, new string[] { $"tcp://localhost:{port2}" });
            // var repo1 = node1.GetService<IBlockRepository>();
            // var repo2 = node2.GetService<IBlockRepository>();
            // var repo3 = node3.GetService<IBlockRepository>();
            // var net1 = node1.GetService<IPeerNetwork>();
            // var net2 = node2.GetService<IPeerNetwork>();
            // var net3 = node3.GetService<IPeerNetwork>();
            // PopulateInitialData(repo1);
        }

        [TestSvm]
        public static ICollection<Instruction> BuildInstructions(KeyPair builderKeys, ICollection<Transaction> transactions)
        {
            var instructions = new HashSet<Instruction>();
            var i1 = new TestInstruction {Data = "test", PublicKey = builderKeys.PublicKey};
            instructions.Add(i1);

            return instructions;
        }

        [TestSvm]
        public static int Test1()
        {
            // new NBlockchain.Models.;
            return 0;
        }
    }
}
