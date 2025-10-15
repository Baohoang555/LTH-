#import Network.Socket
import Control.Concurrent (forkIO)

main :: IO ()
main = do
    -- Bước 1 & 2: Tạo, gắn và lắng nghe trên socket
    sock <- socket AF_INET Stream defaultProtocol
    bind sock (SockAddrInet 8080 0) -- Gắn vào cổng 8080
    listen sock 5 -- Lắng nghe tối đa 5 kết nối chờ

    putStrLn "Server is listening on port 8080"
    mainLoop sock

mainLoop :: Socket -> IO ()
mainLoop sock = do
    -- Bước 3: Chấp nhận kết nối, tạo ra một socket mới cho client
    (conn, addr) <- accept sock
    putStrLn $ "Accepted connection from " ++ show addr

    -- Bước 4: Tạo một luồng riêng để xử lý client này
    forkIO (handleClient conn)

    -- Quay lại vòng lặp để chờ client tiếp theo
    mainLoop sock

-- Hàm này sẽ chạy trong một luồng riêng
handleClient :: Socket -> IO ()
handleClient conn = do
    -- Bước 5: Giao tiếp với client
    msg <- recv conn 1024 -- Nhận dữ liệu
    -- Xử lý logic game ở đây...
    sendAll conn "Phản hồi từ server" -- Gửi dữ liệu

    -- Bước 6: Đóng kết nối khi xong
    close conn