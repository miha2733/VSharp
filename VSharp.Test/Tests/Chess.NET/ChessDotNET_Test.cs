using System;
using System.Collections.Generic;
using System.Linq;
using ChessDotNet;
using NUnit.Framework;

namespace VSharp.Test
{
    [TestSvmFixture]
    public class ChessDotNET_Test
    {
        [TestSvm]
        // [Ignore("You are not prepared!")]
        public static bool FullTest()
        {
            var game = new ChessGame();
            var currentPlayerWhite = game.WhoseTurn;
            Piece pieceAtA1 = game.GetPieceAt(new Position("A1"));
            var piece = pieceAtA1.GetFenCharacter();
            var e2e4 = new Move("E2", "E4", Player.White);
            bool isValid = game.IsValidMove(e2e4);
            MoveType type = game.ApplyMove(e2e4, false);

            var isBlackInCheck = game.IsInCheck(Player.Black);
            var currentPlayerBlack = game.WhoseTurn;

            IEnumerable<Move> validMoves = game.GetValidMoves(Player.Black);
            var validMovesCount = validMoves.Count();
            bool hasValidMoves = game.HasAnyValidMoves(Player.Black);
            return isValid && type != MoveType.Invalid && !isBlackInCheck && currentPlayerWhite == Player.White &&
                   currentPlayerBlack == Player.Black && validMovesCount != 0 && hasValidMoves;
        }

        [TestSvm]
        public static bool CreateGame()
        {
            var game = new ChessGame();
            return game.DrawClaimed;
        }

    }
}
