import Test.Hspec
import Core.Types
import Core.Board
import Core.AI

main :: IO ()
main = hspec $ do
  describe "Board Tests" $ do
    it "tạo bàn cờ rỗng 6x7" $ do
      length emptyBoard `shouldBe` 6
      length (head emptyBoard) `shouldBe` 7
    
    it "thả quân vào cột trống" $ do
      let result = dropPiece 3 Red emptyBoard
      result `shouldSatisfy` (/= Nothing)
    
    it "thả quân vào cột không tồn tại" $ do
      dropPiece 7 Red emptyBoard `shouldBe` Nothing
      dropPiece (-1) Red emptyBoard `shouldBe` Nothing
    
    it "kiểm tra thắng ngang" $ do
      let b1 = dropPiece 0 Red emptyBoard
          b2 = b1 >>= dropPiece 1 Red
          b3 = b2 >>= dropPiece 2 Red
          b4 = b3 >>= dropPiece 3 Red
      case b4 of
        Just board -> checkWin Red board `shouldBe` True
        Nothing -> expectationFailure "Không thả được quân"
    
    it "kiểm tra thắng dọc" $ do
      let moves = [0, 1, 0, 1, 0, 1, 0] -- Red thắng cột 0
          board = foldl (\b (c, p) -> b >>= \brd -> dropPiece c p brd) 
                        (Just emptyBoard) 
                        (zip moves (cycle [Red, Yellow]))
      case board of
        Just b -> checkWin Red b `shouldBe` True
        Nothing -> expectationFailure "Không thả được quân"
    
    it "validMoves trả về đúng cột trống" $ do
      validMoves emptyBoard `shouldBe` [0..6]
    
    it "isFull phát hiện bàn đầy" $ do
      isFull emptyBoard `shouldBe` False

  describe "AI Tests" $ do
    it "AI chọn được nước đi" $ do
      let move = getBestMove Yellow emptyBoard
      move `shouldSatisfy` (/= Nothing)
    
    it "AI chặn nước thắng của đối thủ" $ do
      -- Red sắp thắng ở cột 3, AI phải chặn
      let b1 = dropPiece 0 Red emptyBoard
          b2 = b1 >>= dropPiece 1 Red
          b3 = b2 >>= dropPiece 2 Red
          aiMove = b3 >>= \board -> getBestMove Yellow board
      aiMove `shouldBe` Just 3
    
    it "AI chọn nước thắng nếu có thể" $ do
      -- Yellow sắp thắng ở cột 3
      let b1 = dropPiece 0 Yellow emptyBoard
          b2 = b1 >>= dropPiece 1 Yellow
          b3 = b2 >>= dropPiece 2 Yellow
          aiMove = b3 >>= \board -> getBestMove Yellow board
      aiMove `shouldBe` Just 3

  describe "Types Tests" $ do
    it "nextPlayer đổi lượt đúng" $ do
      nextPlayer Red `shouldBe` Yellow
      nextPlayer Yellow `shouldBe` Red